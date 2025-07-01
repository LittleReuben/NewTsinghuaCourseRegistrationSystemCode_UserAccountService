package Impl


import Objects.UserAccountService.UserInfo
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.fetchUserInfoByToken
import Utils.UserAccountProcess.validateAdminToken
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.UserRole
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryAllUsersMessagePlanner(
                                        adminToken: String,
                                        role: UserRole,
                                        override val planContext: PlanContext
                                      ) extends Planner[List[UserInfo]] {
  val logger = LoggerFactory.getLogger(s"${this.getClass.getSimpleName}_${planContext.traceID.id}")

  override def plan(using planContext: PlanContext): IO[List[UserInfo]] = {
    for {
      // Step 1: Validate admin token
      _ <- IO(logger.info(s"开始验证管理员的Token: ${adminToken}"))
      isAdmin <- validateAdminToken(adminToken)
      _ <- IO(if (!isAdmin) logger.info("管理员Token验证失败，权限不足") else logger.info("管理员Token验证成功"))
      _ <- if (!isAdmin) IO.raiseError(new IllegalStateException("权限不足")) else IO.unit

      // Step 2: Query users by role
      _ <- IO(logger.info(s"开始查询用户信息，根据角色过滤: ${role}"))
      userList <- queryUsersByRole(role)

      // Step 3: Log and return the filtered user list
      _ <- IO(logger.info(s"查询到的符合条件的用户数量: ${userList.length}"))
    } yield userList
  }

  private def queryUsersByRole(role: UserRole)(using PlanContext): IO[List[UserInfo]] = {
    logger.info(s"准备执行数据库查询，筛选用户角色为: ${role}")
    // SQL query for filtering users by role
    val sqlQuery =
      s"""
        SELECT user_id, user_name, account_name, password, role
        FROM ${schemaName}.user_account_table
        WHERE role = ?;
      """
    val parameters = List(SqlParameter("String", role.toString))
    logger.info(s"数据库查询SQL: ${sqlQuery}")

    // Executing the query and decoding the results to UserInfo
    readDBRows(sqlQuery, parameters).map { rows =>
      rows.map { row =>
        val userInfo = UserInfo(
          userID = decodeField[Int](row, "user_id"),
          userName = decodeField[String](row, "user_name"),
          accountName = decodeField[String](row, "account_name"),
          password = decodeField[String](row, "password"),
          role = UserRole.fromString(decodeField[String](row, "role"))
        )
        logger.info(s"解码得到用户信息: ${userInfo}")
        userInfo
      }
    }
  }
}