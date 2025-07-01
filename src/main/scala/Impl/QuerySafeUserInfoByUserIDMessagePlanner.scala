package Impl


import Common.API.{PlanContext, Planner}
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Objects.UserAccountService.SafeUserInfo
import cats.effect.IO
import org.slf4j.LoggerFactory
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
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.UserRole

case class QuerySafeUserInfoByUserIDMessagePlanner(
                                                    userID: Int,
                                                    override val planContext: PlanContext
                                                  ) extends Planner[Option[SafeUserInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[Option[SafeUserInfo]] = {
    for {
      // Step 1: Log the process of fetching SafeUserInfo
      _ <- IO(logger.info(s"[QuerySafeUserInfoByUserID] 开始查询安全用户信息，用户ID为: ${userID}"))
      
      // Step 2: Fetch Safe User Info from `fetchSafeUserInfoByID`
      safeUserInfoOption <- fetchSafeUserInfoByID(userID)
      _ <- IO(logger.info(s"[QuerySafeUserInfoByUserID] 查询完成，是否找到用户信息: ${safeUserInfoOption.isDefined}"))
      
      // Step 3: Check the result and log the output
      returnValue <- safeUserInfoOption match {
        case Some(safeUserInfo) =>
          IO {
            logger.info(s"[QuerySafeUserInfoByUserID] 用户信息找到，用户ID: ${safeUserInfo.userID}, 用户名: ${safeUserInfo.userName}")
            Some(safeUserInfo)
          }
        case None =>
          IO {
            logger.warn(s"[QuerySafeUserInfoByUserID] 查询失败，用户ID: ${userID}不存在。")
            None
          }
      }
    } yield returnValue
  }
}