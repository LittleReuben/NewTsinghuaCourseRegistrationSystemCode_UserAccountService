package Impl

import Objects.UserAccountService.UserInfo
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Utils.UserAccountProcess.validateAdminToken
import Utils.UserAccountProcess.recordUserAccountOperationLog
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.validateAccountNameUniqueness
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
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Utils.UserAccountProcess.fetchUserInfoByToken
import Objects.UserAccountService.SafeUserInfo

case class CreateUserAccountMessagePlanner(
    adminToken: String,
    name: String,
    accountName: String,
    password: String,
    role: UserRole,
    override val planContext: PlanContext
) extends Planner[UserInfo] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[UserInfo] = {
    for {
      _ <- IO(logger.info("开始执行 CreateUserAccountMessagePlanner。"))

      // Step 1: 验证 adminToken 是否有效并具有权限
      _ <- IO(logger.info(s"验证 adminToken: ${adminToken} 是否属于超级管理员"))
      isAdminTokenValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminTokenValid) {
        IO.raiseError(new IllegalArgumentException("权限不足"))
      } else IO.unit

      // Step 2: 确保 accountName 唯一
      _ <- IO(logger.info(s"验证账号名的唯一性: ${accountName}"))
      isAccountNameUnique <- validateAccountNameUniqueness(accountName)
      _ <- if (!isAccountNameUnique) {
        IO.raiseError(new IllegalArgumentException("账号名重复"))
      } else IO.unit

      // Step 3: 创建新的用户记录
      _ <- IO(logger.info(s"准备在数据库中创建用户记录。name: ${name}, accountName: ${accountName}, role: ${role.toString}"))
      generatedUserID <- createUserAccountInDB(name, accountName, password, role)

      // Step 4: 记录日志
      _ <- IO(logger.info(s"记录用户创建操作日志, targetUserID: ${generatedUserID}"))
      _ <- recordUserAccountOperationLog("增加", generatedUserID, s"新增用户，AccountName: ${accountName}, Role: ${role.toString}")

      // Step 5: 构造返回的 UserInfo
      _ <- IO(logger.info(s"通过生成的用户ID(${generatedUserID})获取完整用户信息。"))
      userInfo <- fetchFullUserInfo(generatedUserID)
    } yield userInfo
  }

  private def createUserAccountInDB(
      name: String,
      accountName: String,
      password: String,
      role: UserRole
  )(using PlanContext): IO[Int] = {
    val sql =
      s"""
          INSERT INTO ${schemaName}.user_account_table (user_name, account_name, password, role)
          VALUES (?, ?, ?, ?) RETURNING user_id
        """
    val parameters = List(
      SqlParameter("String", name),
      SqlParameter("String", accountName),
      SqlParameter("String", password), // Password 的加密应在调用前完成
      SqlParameter("String", role.toString)
    )
    for {
      _ <- IO(logger.info(s"执行新增用户记录的 SQL: ${sql}"))
      newUserID <- readDBInt(sql, parameters)
      _ <- IO(logger.info(s"用户记录创建成功，生成 UserID: ${newUserID}"))
    } yield newUserID
  }

  private def fetchFullUserInfo(userID: Int)(using PlanContext): IO[UserInfo] = {
    val sql =
      s"""
         SELECT user_id, user_name, account_name, password, role
         FROM ${schemaName}.user_account_table
         WHERE user_id = ?
      """
    val parameters = List(SqlParameter("Int", userID.toString))
    
    for {
      userJson <- readDBJson(sql, parameters)
      userInfo <- IO {
        UserInfo(
          userID = decodeField[Int](userJson, "user_id"),
          userName = decodeField[String](userJson, "user_name"),
          accountName = decodeField[String](userJson, "account_name"),
          password = decodeField[String](userJson, "password"),
          role = UserRole.fromString(decodeField[String](userJson, "role"))
        )
      }
      _ <- IO(logger.info(s"成功构造用户信息对象: ${userInfo}"))
    } yield userInfo
  }
}