package Impl


import Objects.UserAccountService.SafeUserInfo
import Utils.UserAccountProcess.fetchSafeUserInfoByToken
import Objects.UserAccountService.UserRole
import Common.API.{PlanContext, Planner}
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

case class QuerySafeUserInfoByTokenMessagePlanner(
  userToken: String,
  override val planContext: PlanContext
) extends Planner[Option[SafeUserInfo]] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[Option[SafeUserInfo]] = {
    for {
      // Step 1: Fetch SafeUserInfo by token
      _ <- IO(logger.info(s"[Step 1] 开始通过 token 查询安全用户信息，token: ${userToken}"))
      safeUserInfoOption <- fetchSafeUserInfoByToken(userToken)

      // Step 2: Validate and return result
      _ <- safeUserInfoOption match {
        case Some(safeUserInfo) =>
          IO(logger.info(s"[Step 2] 查询成功，用户信息: ${safeUserInfo}"))
        case None =>
          IO(logger.warn(s"[Step 2] 查询失败，用户不存在或 token 无效"))
      }
    } yield safeUserInfoOption
  }
}