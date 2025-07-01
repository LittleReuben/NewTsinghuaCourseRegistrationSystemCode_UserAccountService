package Impl


import Objects.UserAccountService.{UserInfo, SafeUserInfo, UserRole}
import Utils.UserAccountProcess.{invalidateUserToken, fetchSafeUserInfoByID, validateAdminToken, recordUserAccountOperationLog, validateAccountNameUniqueness, fetchUserInfoByID}
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
import Utils.UserAccountProcess.invalidateUserToken
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Utils.UserAccountProcess.validateAdminToken
import Utils.UserAccountProcess.recordUserAccountOperationLog
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.validateAccountNameUniqueness
import Utils.UserAccountProcess.fetchUserInfoByToken
import Objects.UserAccountService.SafeUserInfo
import Objects.SystemLogService.SystemLogEntry
import Utils.UserAccountProcess.fetchUserInfoByID
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Utils.UserAccountProcess.fetchUserInfoByID

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
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 验证管理员权限，adminToken: ${adminToken}"))
      isAdmin <- validateAdminToken(adminToken)
      _ <- if (!isAdmin) {
        IO.raiseError(new IllegalArgumentException("权限不足：adminToken 无效或不是超级管理员权限"))
      } else IO.unit
      _ <- IO(logger.info("[UpdateUserAccountMessagePlanner] 管理员权限验证通过"))

      // Step 2: 查询用户信息
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 查询用户信息，用户ID: ${userID}"))
      maybeUserInfo <- fetchUserInfoByID(userID)
      userInfo <- maybeUserInfo match {
        case Some(info) => IO.pure(info)
        case None =>
          IO.raiseError(new IllegalArgumentException(s"用户ID [${userID}] 不存在"))
      }

      // Step 3: 验证新账号名是否唯一（如果 newAccountName 存在）
      _ <- newAccountName match {
        case Some(name) =>
          IO(logger.info(s"[UpdateUserAccountMessagePlanner] 验证账号名是否唯一，newAccountName: ${name}")) >>
          validateAccountNameUniqueness(name).flatMap { isUnique =>
            if (!isUnique) IO.raiseError(new IllegalArgumentException(s"账号名 [${name}] 已存在"))
            else IO.unit
          }
        case None => IO.unit
      }

      // Step 4: 更新用户信息
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 更新用户信息，用户ID: ${userID}"))
      updateOperations <- IO {
        List(
          newName.map(name => ("user_name", name)),
          newAccountName.map(accountName => ("account_name", accountName)),
          newPassword.map(password => ("password", password))
        ).flatten
      }
      updateSql <- IO {
        s"""
        UPDATE ${schemaName}.user_account_table
        SET ${updateOperations.map { case (field, _) => s"${field} = ?" }.mkString(", ")}
        WHERE user_id = ?;
      """
      }
      updateParameters <- IO {
        updateOperations.map {
          case (_, value) => SqlParameter("String", value)
        } :+ SqlParameter("Int", userID.toString)
      }
      _ <- writeDB(updateSql, updateParameters)
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 用户信息更新完成，用户ID: ${userID}"))

      // Step 5: 强制登出
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 强制登出用户登录状态，用户ID: ${userID}"))
      _ <- invalidateUserToken(userID)

      // Step 6: 记录操作日志
      operationDetails <- IO {
        s"更新用户信息: ${updateOperations.map {
          case (field, value) => s"${field} -> ${value}"
        }.mkString(", ")}"
      }
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 记录操作日志，操作详情: ${operationDetails}"))
      _ <- recordUserAccountOperationLog("修改", userID, operationDetails)

      // Step 7: 获取更新后的用户信息
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 查询更新后的用户信息，用户ID: ${userID}"))
      updatedUserInfo <- fetchUserInfoByID(userID).flatMap {
        case Some(user) => IO.pure(user)
        case None => IO.raiseError(new IllegalStateException(s"无法找到更新后的用户信息"))
      }

      // Step 8: 返回结果
      _ <- IO(logger.info(s"[UpdateUserAccountMessagePlanner] 更新完成，返回用户信息: ${updatedUserInfo}"))
    } yield updatedUserInfo
  }
}