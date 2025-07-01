package Impl


import Objects.UserAccountService.UserInfo
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.invalidateUserToken
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Utils.UserAccountProcess.validateAdminToken
import Utils.UserAccountProcess.recordUserAccountOperationLog
import Utils.UserAccountProcess.fetchUserInfoByID
import Utils.UserAccountProcess.validateAccountNameUniqueness
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
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
import Objects.SystemLogService.SystemLogEntry
import Utils.UserAccountProcess.fetchUserInfoByID
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UpdateUserAccountMessagePlanner(
  adminToken: String,
  userID: Int,
  newName: Option[String],
  newAccountName: Option[String],
  newPassword: Option[String],
  override val planContext: PlanContext
) extends Planner[UserInfo] {
  
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[UserInfo] = {
    for {
      // Step 1: 验证管理员权限
      isAdminValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminValid)
        IO.raiseError(new IllegalAccessException("权限不足，adminToken验证失败"))
      else IO(logger.info(s"管理员权限验证通过，adminToken: ${adminToken}"))

      // Step 2: 查询目标用户的信息
      maybeUserInfo <- fetchUserInfoByID(userID)
      userInfo <- maybeUserInfo match {
        case Some(u) => IO.pure(u)
        case None => IO.raiseError(new IllegalArgumentException("用户不存在"))
      }
      _ <- IO(logger.info(s"目标用户信息查询成功: ${userInfo}"))

      // Step 3: 验证 newAccountName 是否唯一（如有）
      _ <- newAccountName match {
        case Some(accountName) =>
          validateAccountNameUniqueness(accountName).flatMap {
            case false =>
              IO.raiseError(new IllegalArgumentException("账号名重复错误"))
            case true =>
              IO(logger.info(s"账号名验证通过，新账号名: ${accountName}"))
          }
        case None =>
          IO(logger.info(s"未传递新账号名，跳过唯一性验证"))
      }

      // Step 4: 更新用户信息至数据库
      updateFields <- IO {
        List(
          newName.map(name => "user_name" -> name),
          newAccountName.map(accountName => "account_name" -> accountName),
          newPassword.map(password => "password" -> password)
        ).flatten
      }

      _ <- if (updateFields.isEmpty)
        IO.raiseError(new IllegalArgumentException("未传递任何更新字段"))
      else IO.unit

      updateSql <- IO {
        s"""
          UPDATE ${schemaName}.user_account_table
          SET ${updateFields.map { case (field, _) => s"${field} = ?" }.mkString(", ")}
          WHERE user_id = ?;
        """
      }
      updateParams <- IO {
        updateFields.map { case (_, value) => SqlParameter("String", value) } :+ SqlParameter("Int", userID.toString)
      }
      updateResult <- writeDB(updateSql, updateParams)
      _ <- IO(logger.info(s"用户信息更新成功，数据库写入结果: ${updateResult}"))

      // Step 5: 使用户登录token失效，强制登出
      logoutResult <- invalidateUserToken(userID)
      _ <- IO(logger.info(s"目标用户强制登出完成，结果: ${logoutResult}"))

      // Step 6: 记录操作日志
      logDetails <- IO(s"更新字段: ${updateFields.map { case (field, value) => s"${field}=${value}" }.mkString(", ")}")
      logResult <- recordUserAccountOperationLog("修改", userID, logDetails)
      _ <- IO(logger.info(s"操作日志记录完成，结果: ${logResult}"))

      // Step 7: 查询更新后的用户信息
      updatedSafeUserInfo <- fetchSafeUserInfoByID(userID)
      updatedUserInfo <- updatedSafeUserInfo match {
        case Some(info) =>
          IO(UserInfo(info.userID, info.userName, info.accountName, newPassword.getOrElse(userInfo.password), info.role))
        case None => IO.raiseError(new IllegalStateException("更新后的用户信息查询失败"))
      }
      _ <- IO(logger.info(s"更新后的用户信息: ${updatedUserInfo}"))
    } yield updatedUserInfo
  }
}