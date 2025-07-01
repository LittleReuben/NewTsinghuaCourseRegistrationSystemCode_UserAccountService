import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.{ParameterList, SqlParameter}
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

def UserAccountProcess()(using PlanContext): IO[Unit] = {
  val logger = LoggerFactory.getLogger(getClass)
  logger.info("开始用户账户处理工具类 - UserAccountProcess.")

  val fetchUsersSQL =
    s"""
      SELECT user_id, username, last_login_time
      FROM ${schemaName}.user_accounts
      WHERE is_active = true;
    """
  val updateLastProcessedSQL =
    s"""
      UPDATE ${schemaName}.user_accounts
      SET last_processed_time = ?
      WHERE user_id = ?;
    """
  val now = DateTime.now()

  for {
    _ <- IO(logger.info(s"生成查询活跃用户的 SQL: ${fetchUsersSQL}"))
    users <- readDBRows(fetchUsersSQL, List())
    _ <- IO(logger.info(s"共查询到 ${users.size} 个活跃用户."))

    updateParams <- IO {
      users.map { userJson =>
        val userID = decodeField[Int](userJson, "user_id")
        val username = decodeField[String](userJson, "username")
        val lastLoginTime = decodeField[Option[DateTime]](userJson, "last_login_time")

        logger.info(s"处理用户: userID=${userID}, username=${username}, lastLoginTime=${lastLoginTime.getOrElse("无记录")}")

        ParameterList(List(
          SqlParameter("DateTime", now.getMillis.toString),
          SqlParameter("Int", userID.toString)
        ))
      }
    }

    _ <- IO(logger.info("开始更新用户的 last_processed_time"))
    _ <- writeDBList(updateLastProcessedSQL, updateParams)
    _ <- IO(logger.info("用户账户处理工具类执行完毕 - UserAccountProcess."))
  } yield ()
}