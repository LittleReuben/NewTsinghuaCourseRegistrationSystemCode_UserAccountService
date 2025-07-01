package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.UserInfo
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.validateAdminToken
import Utils.UserAccountProcess.validateAccountNameUniqueness
import Utils.UserAccountProcess.recordUserAccountOperationLog
import org.slf4j.LoggerFactory
import cats.effect.IO
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
import org.joda.time.DateTime
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Objects.SystemLogService.SystemLogEntry
import Utils.UserAccountProcess.fetchUserInfoByToken
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
import Objects.UserAccountService.SafeUserInfo
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class CreateUserAccountMessagePlanner(
    adminToken: String,
    name: String,
    accountName: String,
    password: String,
    role: UserRole,
    override val planContext: PlanContext
) extends Planner[UserInfo] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[UserInfo] = {
    for {
      // Step 1: Validate admin token
      _ <- IO(logger.info(s"[Step 1] Validating adminToken: ${adminToken}"))
      _ <- validateAdminToken(adminToken).flatMap(isAdmin =>
        if (!isAdmin) IO.raiseError(new Exception("权限不足")) else IO.unit
      )

      // Step 2: Validate account name uniqueness
      _ <- IO(logger.info(s"[Step 2] Checking accountName uniqueness: ${accountName}"))
      _ <- validateAccountNameUniqueness(accountName).flatMap(isUnique =>
        if (!isUnique) IO.raiseError(new Exception("账号名重复")) else IO.unit
      )

      // Step 3: Insert new user into UserAccountTable and get new userID
      _ <- IO(logger.info(s"[Step 3] Inserting new user: name=${name}, accountName=${accountName}, role=${role.toString}"))
      userID <- insertUserRecord(name, accountName, password, role)

      // Step 4: Record operation log
      _ <- IO(logger.info(s"[Step 4] Recording operation log for created userID: ${userID}"))
      _ <- recordUserAccountOperationLog("增加", userID, s"创建用户: name=${name}, accountName=${accountName}, role=${role.toString}")

      // Step 5: Construct and return UserInfo object
      _ <- IO(logger.info(s"[Step 5] Constructing UserInfo object for userID: ${userID}"))
      userInfo <- constructUserInfo(userID)
    } yield userInfo
  }

  private def insertUserRecord(
      name: String,
      accountName: String,
      password: String,
      role: UserRole
  )(using PlanContext): IO[Int] = {
    val sql =
      s"""
         INSERT INTO ${schemaName}.user_account_table (user_name, account_name, password, role)
         VALUES (?, ?, ?, ?)
         RETURNING user_id;
       """
    val parameters = List(
      SqlParameter("String", name),
      SqlParameter("String", accountName),
      SqlParameter("String", password),
      SqlParameter("String", role.toString)
    )

    for {
      _ <- IO(logger.info(s"[InsertUserRecord] SQL: $sql"))
      _ <- IO(logger.info(s"[InsertUserRecord] Parameters: ${parameters.map(_.value).mkString(", ")}"))
      userID <- readDBInt(sql, parameters)
      _ <- IO(logger.info(s"[InsertUserRecord] Inserted userID: $userID"))
    } yield userID
  }

  private def constructUserInfo(userID: Int)(using PlanContext): IO[UserInfo] = {
    val sql =
      s"""
         SELECT user_id, user_name, account_name, password, role
         FROM ${schemaName}.user_account_table
         WHERE user_id = ?;
       """
    val parameters = List(SqlParameter("Int", userID.toString))

    for {
      _ <- IO(logger.info(s"[ConstructUserInfo] SQL: $sql"))
      _ <- IO(logger.info(s"[ConstructUserInfo] Parameters: ${parameters.map(_.value).mkString(", ")}"))
      json <- readDBJson(sql, parameters)
      userInfo <- IO {
        UserInfo(
          userID = decodeField[Int](json, "user_id"),
          userName = decodeField[String](json, "user_name"),
          accountName = decodeField[String](json, "account_name"),
          password = decodeField[String](json, "password"),
          role = UserRole.fromString(decodeField[String](json, "role"))
        )
      }
      _ <- IO(logger.info(s"[ConstructUserInfo] Constructed UserInfo: ${userInfo}"))
    } yield userInfo
  }
}