package Impl


import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.effect.IO
import org.slf4j.LoggerFactory
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
import Objects.UserAccountService.{SafeUserInfo, UserRole}
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QuerySafeUserInfoByUserIDListMessagePlanner(
  userIDList: List[Int],
  override val planContext: PlanContext
) extends Planner[List[SafeUserInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[SafeUserInfo]] = {
    for {
      // Step 1: Log the input userID list
      _ <- IO(logger.info(s"[QuerySafeUserInfo] 查询的用户ID列表: ${userIDList.mkString(", ")}"))

      // Step 1.1: Fetch user information for each userID
      safeUserInfoListOptional <- fetchAllSafeUserInfo(userIDList)
      _ <- IO(logger.info(s"[QuerySafeUserInfo] 成功获取批量用户信息，结果数量: ${safeUserInfoListOptional.size}"))

      // Step 2: Sort results based on the input userID list order
      sortedSafeUserInfoList <- IO {
        sortResults(safeUserInfoListOptional, userIDList)
      }
      _ <- IO(logger.info(s"[QuerySafeUserInfo] 用户信息排序完成，返回结果的顺序与输入顺序一致"))
    } yield sortedSafeUserInfoList
  }

  /**
   * Fetch user information for each userID
   *
   * @param userIDList List of user IDs
   * @return List containing optional SafeUserInfo for each userID
   */
  private def fetchAllSafeUserInfo(userIDList: List[Int])(using PlanContext): IO[List[Option[SafeUserInfo]]] = {
    IO(logger.info(s"[QuerySafeUserInfo] 开始批量调用 fetchSafeUserInfoByID 方法查询用户信息")) >>
      userIDList.traverse { userID =>
        fetchSafeUserInfoByID(userID).handleErrorWith { error =>
          IO {
            logger.error(s"[QuerySafeUserInfo] 查询用户ID ${userID} 的信息时出错: ${error.getMessage}")
            None
          }
        }
      }
  }

  /**
   * Sort results based on the input userID list order
   *
   * @param fetchedResults List of optional user information fetched from the database
   * @param userIDList Original input userID list
   * @return Sorted list of SafeUserInfo matching the input order
   */
  private def sortResults(fetchedResults: List[Option[SafeUserInfo]], userIDList: List[Int]): List[SafeUserInfo] = {
    val resultMap = fetchedResults.flatten.map(safeUserInfo => safeUserInfo.userID -> safeUserInfo).toMap

    userIDList.map { userID =>
      resultMap.getOrElse(userID, {
        logger.warn(s"[QuerySafeUserInfo] 用户ID ${userID} 不存在，未包含在返回的SafeUserInfo中")
        throw new IllegalStateException(s"SafeUserInfo for userID [${userID}] not found.")
      })
    }
  }
}