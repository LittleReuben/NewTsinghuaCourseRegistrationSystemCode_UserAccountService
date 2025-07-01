package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.{UserRole, SafeUserInfo}
import Utils.UserAccountProcess.fetchSafeUserInfoByToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
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
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.UserAccountService.UserRole

case class QuerySafeUserInfoByTokenMessagePlanner(
                                                   userToken: String,
                                                   override val planContext: PlanContext
                                                 ) extends Planner[Option[SafeUserInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[Option[SafeUserInfo]] = {
    for {
      _ <- IO(logger.info(s"开始根据用户token查询安全用户信息, token: ${userToken}"))
      // Step 1: Fetch user information by token
      safeUserInfoOption <- fetchSafeUserInfo(userToken)
      _ <- IO(logger.info(s"已根据token完成查询，结果为: ${safeUserInfoOption.fold("查询失败，用户不存在或token无效")(info => s"用户信息: ${info}")}"))
      // Step 2: If user info is found, return it; else log an error message
      result <- if (safeUserInfoOption.isDefined) {
        IO(logger.info("成功查询到用户信息，准备返回用户数据"))
          .map(_ => safeUserInfoOption)
      } else {
        IO(logger.error(s"[错误]用户不存在或token无效: ${userToken}"))
          .map(_ => None)
      }
    } yield result
  }

  private def fetchSafeUserInfo(userToken: String)(using PlanContext): IO[Option[SafeUserInfo]] = {
    fetchSafeUserInfoByToken(userToken)
  }
}