package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.{UserInfo, UserRole}
import Utils.UserAccountProcess.{fetchUserInfoByToken, validateAdminToken}
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
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
import Objects.UserAccountService.UserInfo
import Utils.UserAccountProcess.fetchUserInfoByToken
import Utils.UserAccountProcess.validateAdminToken
import Objects.UserAccountService.UserRole
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.UserAccountService.UserRole

case class QueryAllUsersMessagePlanner(
                                         adminToken: String,
                                         role: UserRole,
                                         override val planContext: PlanContext
                                       ) extends Planner[List[UserInfo]] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[UserInfo]] = {
    for {
      // Step 1: Validate adminToken
      _ <- IO(logger.info(s"[Step 1] 开始验证管理员 Token：${adminToken}"))
      isAdminValid <- validateAdminToken(adminToken)
      _ <- IO(logger.info(s"[Step 1.1] 管理员 Token 验证结果：${isAdminValid}"))

      // Check if adminToken validation failed
      _ <- if (!isAdminValid) {
        IO.raiseError(new IllegalAccessException("[Step 1.1] 权限不足"))
      } else IO.unit

      // Step 2: Query all users by role
      _ <- IO(logger.info(s"[Step 2] 开始从数据库按角色 ${role.toString} 筛选用户记录"))
      userList <- queryUsersByRole(role)
      _ <- IO(logger.info(s"[Step 2.1] 查询结果：${userList.size} 条记录"))

    } yield userList
  }

  /**
   * Helper function to query users by role
   */
  private def queryUsersByRole(role: UserRole)(using PlanContext): IO[List[UserInfo]] = {
    val queryLogger = LoggerFactory.getLogger("QueryUsersByRole")
    queryLogger.info(s"开始创建根据角色 ${role.toString} 筛选用户的数据库 SQL 指令")

    val sql =
      s"""
         |SELECT user_id, user_name, account_name, password, role
         |FROM ${schemaName}.user_account_table
         |WHERE role = ?;
       """.stripMargin
    val parameters = List(SqlParameter("String", role.toString))

    queryLogger.info(s"指令为: ${sql}")
    queryLogger.info("开始执行查询用户记录的数据库指令")

    readDBRows(sql, parameters).map { rows =>
      rows.map { json =>
        val userRoleStr = decodeField[String](json, "role")
        UserInfo(
          userID = decodeField[Int](json, "user_id"),
          userName = decodeField[String](json, "user_name"),
          accountName = decodeField[String](json, "account_name"),
          password = decodeField[String](json, "password"),
          role = UserRole.fromString(userRoleStr) // Validate UserRole here
        )
      }
    }
  }
}