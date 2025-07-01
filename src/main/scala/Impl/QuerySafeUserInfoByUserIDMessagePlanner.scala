package Impl


import Objects.UserAccountService.SafeUserInfo
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Objects.UserAccountService.UserRole
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
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
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QuerySafeUserInfoByUserIDMessagePlanner(
                                                   userID: Int,
                                                   override val planContext: PlanContext
                                                 ) extends Planner[Option[SafeUserInfo]] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[Option[SafeUserInfo]] = {
    for {
      // Step 1: Log the start of the process
      _ <- IO(logger.info(s"[QuerySafeUserInfoByUserID] 开始查询安全用户信息，用户ID: ${userID}"))

      // Step 2: Attempt to fetch SafeUserInfo from database
      safeUserInfoOpt <- fetchSafeUserInfoByID(userID)
      _ <- IO(logger.info(
        s"[QuerySafeUserInfoByUserID] 查询数据库结束，用户ID=${userID} " +
          s"结果是否存在: ${safeUserInfoOpt.isDefined}"
      ))

      // Step 3: Handle the None case and log appropriately
      result <- safeUserInfoOpt match {
        case Some(safeUserInfo) =>
          IO {
            logger.info(s"[QuerySafeUserInfoByUserID] 查询成功，返回的SafeUserInfo: ${safeUserInfo.asJson.noSpaces}")
            Some(safeUserInfo)
          }
        case None =>
          IO {
            logger.warn(s"[QuerySafeUserInfoByUserID] 用户ID=${userID} 不存在，返回None")
            None
          }
      }
    } yield result
  }
}