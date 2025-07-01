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
import Objects.UserAccountService.UserRole
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QuerySafeUserInfoByUserIDListMessagePlanner(
    userIDList: List[Int],
    override val planContext: PlanContext
) extends Planner[List[SafeUserInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[SafeUserInfo]] = {
    for {
      // Step 1: Log the input list and start the fetching process
      _ <- IO(logger.info(s"开始查询用户安全信息，输入的userIDList长度为: ${userIDList.length}"))
      
      // Step 2: Fetch safe user info for each userID in the input list, using the helper method
      userInfoResults <- fetchSafeUserInfoList(userIDList)
      
      // Step 3: Log non-existing user data (if necessary)
      _ <- logMissingUsers(userIDList, userInfoResults)
      
      // Step 4: Collect results, ensuring input order is preserved
      result = userInfoResults.flatten
      _ <- IO(logger.info(s"成功收集查询结果的安全用户信息总数为: ${result.size}"))
    } yield result
  }

  /**
   * Sub-function to fetch user info list while preserving input order.
   */
  private def fetchSafeUserInfoList(userIDList: List[Int])(using PlanContext): IO[List[Option[SafeUserInfo]]] = {
    userIDList.traverse(fetchSafeUserInfoByID)
  }

  /**
   * Sub-function to log missing users whose information couldn't be found.
   */
  private def logMissingUsers(userIDList: List[Int], userInfoResults: List[Option[SafeUserInfo]])(using PlanContext): IO[Unit] = {
    val missingUserIDs = userIDList.zip(userInfoResults).collect {
      case (userID, None) => userID
    }
    if (missingUserIDs.nonEmpty) {
      IO(logger.warn(s"以下用户ID的安全信息无法找到: ${missingUserIDs.mkString(", ")}"))
    } else {
      IO.unit
    }
  }
}