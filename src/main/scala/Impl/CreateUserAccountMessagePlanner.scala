package Impl


import Objects.UserAccountService.UserInfo
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Utils.UserAccountProcess.validateAdminToken
import Utils.UserAccountProcess.recordUserAccountOperationLog
import Utils.UserAccountProcess.validateAccountNameUniqueness
import Objects.SystemLogService.SystemLogEntry
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import cats.effect.IO
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
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
import Utils.UserAccountProcess.fetchUserInfoByToken
import Objects.UserAccountService.SafeUserInfo
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.UserAccountService.SafeUserInfo

case class CreateUserAccountMessagePlanner(
    adminToken: String,
    name: String,
    accountName: String,
    password: String,
    role: UserRole,
    override val planContext: PlanContext
) extends Planner[UserInfo] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[UserInfo] = {
    for {
      // Step 1: Validate adminToken
      _ <- IO(logger.info(s"开始验证管理员 Token：${adminToken}"))
      isAdminValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminValid) IO.raiseError(new IllegalArgumentException("权限不足")) else IO.unit
      _ <- IO(logger.info("管理员 Token 验证通过"))

      // Step 2: Validate accountName uniqueness
      _ <- IO(logger.info(s"开始验证账号名是否唯一：${accountName}"))
      isAccountUnique <- validateAccountNameUniqueness(accountName)
      _ <- if (!isAccountUnique) IO.raiseError(new IllegalArgumentException("账号名重复")) else IO.unit
      _ <- IO(logger.info("账号名验证通过"))

      // Step 3: Create user in UserAccountTable
      _ <- IO(logger.info(s"开始创建用户账号，Name: ${name}, AccountName: ${accountName}, Role: ${role.toString}"))
      userID <- createUserAccount()
      _ <- IO(logger.info(s"成功创建用户账号，生成的用户 ID: ${userID}"))

      // Step 4: Record operation log
      _ <- IO(logger.info(s"开始记录操作日志"))
      logMessage = s"创建用户账号，Name: ${name}, AccountName: ${accountName}, Password: [已加密], Role: ${role.toString}"
      _ <- recordUserAccountOperationLog("增加", userID, logMessage)
      _ <- IO(logger.info("操作日志记录成功"))

      // Step 5: Construct UserInfo object
      _ <- IO(logger.info("开始构造返回的用户信息对象"))
      userInfo <- IO {
        UserInfo(userID, name, accountName, password, role)
      }
      _ <- IO(logger.info(s"成功构造返回的用户信息对象：${userInfo}"))

    } yield userInfo
  }

  private def createUserAccount(using PlanContext): IO[Int] = {
    for {
      // SQL构造和参数准备
      sql <- IO {
        s"""
         INSERT INTO ${schemaName}.user_account_table
         (user_name, account_name, password, role)
         VALUES (?, ?, ?, ?)
         RETURNING user_id;
       """
      }
      parameters <- IO {
        List(
          SqlParameter("String", name),
          SqlParameter("String", accountName),
          SqlParameter("String", password),  // 密码这个字段应已加密，假设它已经由其他逻辑加密后传入
          SqlParameter("String", role.toString)
        )
      }

      _ <- IO(logger.info(s"[createUserAccount] 执行数据库插入语句：${sql}"))
      
      userID <- readDBInt(sql, parameters)
      _ <- IO(logger.info(s"[createUserAccount] 成功插入用户账号记录，返回的用户 ID: ${userID}"))
    } yield userID
  }
}