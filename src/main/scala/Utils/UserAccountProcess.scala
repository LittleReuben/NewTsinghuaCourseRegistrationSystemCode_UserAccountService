import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime

def UserAccountProcess()(using PlanContext): IO[Unit] = {
  val logger = LoggerFactory.getLogger(getClass)
  logger.info("[UserAccountProcess] 开始处理用户账户信息")

  val userInfoQuery =
    s"""
      SELECT user_id, account_status, last_login, login_attempts
      FROM ${schemaName}.user_account
      WHERE account_status = 'ACTIVE';
    """

  for {
    // Step 1: 获取活动账户信息
    _ <- IO(logger.info(s"[Step 1] 查询所有活动用户账户信息 SQL: ${userInfoQuery}"))
    activeUsers <- readDBRows(userInfoQuery, List.empty)
    _ <- IO(logger.info(s"[Step 1] 查询返回 ${activeUsers.size} 条活动账户记录"))

    // Step 2: 检查每个账户是否需要处理
    accountsToProcess <- IO {
      activeUsers.map { row =>
        val userID = decodeField[String](row, "user_id")
        val lastLogin = decodeField[DateTime](row, "last_login")
        val loginAttempts = decodeField[Int](row, "login_attempts")
        logger.debug(s"[Step 2] 用户 [userID=${userID}] 最后登录时间: ${lastLogin}, 登录尝试次数: ${loginAttempts}")
        (userID, lastLogin, loginAttempts)
      }.filter { case (_, lastLogin, _) =>
        val expiryTime = DateTime.now().minusDays(30)
        logger.debug(s"[Step 2] 当前时间: ${DateTime.now()}, 过期时间: ${expiryTime}")
        lastLogin.isBefore(expiryTime)
      }
    }
    _ <- IO(logger.info(s"[Step 2] 需要处理的账户数量: ${accountsToProcess.size}"))

    // Step 3: 更新数据库中的用户状态和登录尝试次数
    _ <- if (accountsToProcess.isEmpty) {
      IO(logger.info("[Step 3] 无账户需要更新，流程结束"))
    } else {
      val updateSQL =
        s"""
          UPDATE ${schemaName}.user_account
          SET account_status = 'INACTIVE', login_attempts = 0
          WHERE user_id = ?;
        """

      val updateParams <- IO {
        accountsToProcess.map { case (userID, _, _) =>
          List(SqlParameter("String", userID))
        }.map(ParameterList)
      }

      IO(logger.info(s"[Step 3] 开始批量更新账户信息 SQL: ${updateSQL}")) >>
      writeDBList(updateSQL, updateParams).void >>
      IO(logger.info(s"[Step 3] 共更新 ${updateParams.size} 条记录完成"))
    }

    _ <- IO(logger.info("[UserAccountProcess] 账户处理任务完成"))
  } yield ()
}