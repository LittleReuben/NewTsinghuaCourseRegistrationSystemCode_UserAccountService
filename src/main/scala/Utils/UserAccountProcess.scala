package Utils

//process plan import 预留标志位，不要删除
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import org.slf4j.LoggerFactory
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Common.API.PlanContext
import Common.Object.SqlParameter
import cats.effect.IO
import cats.implicits.*
import Common.API.{PlanContext, Planner}
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.API.{PlanContext}
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Objects.UserAccountService.UserInfo
import Utils.UserAccountProcess.fetchUserInfoByID
import Utils.UserAccountProcess.recordUserAccountOperationLog
import Utils.UserAccountProcess.fetchSafeUserInfoByID
import Objects.SystemLogService.SystemLogEntry
import Utils.UserAccountProcess.fetchUserInfoByToken
import Common.DBAPI.{readDBJsonOptional, decodeField, SqlParameter}
import Common.DBAPI.{readDBJsonOptional, decodeField}
import UserAuthService.UserAuthService_ProcessRoot.ValidateTokenValidity
import APIs.UserAuthService.ValidateTokenValidityMessage

case object UserAccountProcess {
  private val logger = LoggerFactory.getLogger(getClass)
  //process plan code 预留标志位，不要删除
  
  def fetchSafeUserInfoByID(userID: Int)(using PlanContext): IO[Option[SafeUserInfo]] = {
  // val logger = LoggerFactory.getLogger("FetchSafeUserInfoLogger")  // 同文后端处理: logger 统一
    logger.info(s"[FetchSafeUserInfo] 正在查询用户ID: ${userID}")
  
    val sqlQuery = s"SELECT user_id, user_name, account_name, role FROM ${schemaName}.user_account_table WHERE user_id = ?"
    val parameters = List(SqlParameter("Int", userID.toString))
  
    for {
      _ <- IO(logger.info(s"[FetchSafeUserInfo] 执行数据库查询 SQL: ${sqlQuery}"))
      result <- readDBJsonOptional(sqlQuery, parameters)
      _ <- IO(logger.info(s"[FetchSafeUserInfo] 数据库查询完成，结果是否存在: ${result.isDefined}"))
      safeUserInfoOptional <- IO {
        result.map { json =>
          val userID = decodeField[Int](json, "user_id")
          val userName = decodeField[String](json, "user_name")
          val accountName = decodeField[String](json, "account_name")
          val roleString = decodeField[String](json, "role")
          val userRole = UserRole.fromString(roleString) // 将字符串转换为枚举类型
          SafeUserInfo(
            userID = userID,
            userName = userName,
            accountName = accountName,
            role = userRole
          )
        }
      }
      _ <- IO(logger.info(s"[FetchSafeUserInfo] 构造SafeUserInfo完成，返回值是否存在: ${safeUserInfoOptional.isDefined}"))
    } yield safeUserInfoOptional
  }
  
  // 修复编译错误: 添加 ValidateTokenValidity 的调用实现
  
  def fetchSafeUserInfoByToken(userToken: String)(using PlanContext): IO[Option[SafeUserInfo]] = {
    for {
      // Step1: Validate token validity
      _ <- IO(logger.info(s"开始验证用户token的有效性: ${userToken}"))
      isValidToken <- ValidateTokenValidityMessage(userToken).send
      _ <- if (!isValidToken) {
        IO(logger.info(s"用户token无效: ${userToken}")) >>
        IO.pure(None)
      } else IO.unit
      
      // Step2: Fetch user basic info from database
      _ <- IO(logger.info(s"用户token有效，准备从数据库中查询用户基本信息"))
      query <- IO(s"SELECT user_id, user_name, account_name, role FROM ${schemaName}.user_account WHERE token = ?")
      parameters <- IO(List(SqlParameter("String", userToken)))
      userInfoJsonOptional <- readDBJsonOptional(query, parameters)
      _ <- IO(logger.info(s"数据查询完成，${userInfoJsonOptional.fold("未查询到用户信息")(info => s"获取到用户信息: ${info}")}"))
      
    } yield userInfoJsonOptional.map { json =>
      SafeUserInfo(
        userID = decodeField[Int](json, "user_id"),
        userName = decodeField[String](json, "user_name"),
        accountName = decodeField[String](json, "account_name"),
        role = UserRole.fromString(decodeField[String](json, "role"))
      )
    }
  }
  
  def fetchUserInfoByToken(userToken: String)(using PlanContext): IO[Option[UserInfo]] = {
  // val logger = LoggerFactory.getLogger("fetchUserInfoByToken")  // 同文后端处理: logger 统一
    logger.info(s"开始执行fetchUserInfoByToken，传入的用户 token: ${userToken}")
  
    // 第一步：验证用户的token是否有效
    val tokenValidationSQL =
      s"""
        SELECT token_id, user_id, expiry_date
        FROM ${schemaName}.user_token_table
        WHERE token = ?;
      """
    val tokenParams = List(SqlParameter("String", userToken))
  
    logger.info(s"准备验证Token的SQL: ${tokenValidationSQL}")
    
    for {
      tokenQueryResult <- readDBJsonOptional(tokenValidationSQL, tokenParams)
      userInfo <- tokenQueryResult match {
        case Some(token) =>
          val expiryDate = decodeField[DateTime](token, "expiry_date")
          val userID = decodeField[Int](token, "user_id")
          
          if (expiryDate.isBefore(DateTime.now())) {
            logger.info(s"Token已过期，expiryDate: ${expiryDate}")
            IO.pure(None)
          } else {
            logger.info(s"Token验证成功，关联用户ID: ${userID}")
  
            // 第二步：通过用户ID查询用户信息
            val userInfoSQL =
              s"""
                SELECT user_id, user_name, account_name, password, role
                FROM ${schemaName}.user_account_table
                WHERE user_id = ?;
              """
            val userParams = List(SqlParameter("Int", userID.toString))
            
            logger.info(s"准备查询用户信息的SQL: ${userInfoSQL}")
  
            for {
              userQueryResult <- readDBJsonOptional(userInfoSQL, userParams)
              fullUserInfo <- userQueryResult match {
                case Some(userRow) =>
                  val userInfo = UserInfo(
                    userID = decodeField[Int](userRow, "user_id"),
                    userName = decodeField[String](userRow, "user_name"),
                    accountName = decodeField[String](userRow, "account_name"),
                    password = decodeField[String](userRow, "password"),
                    role = UserRole.fromString(decodeField[String](userRow, "role"))
                  )
                  logger.info(s"成功获取到用户信息: ${userInfo}")
                  IO.pure(Some(userInfo))
                case None =>
                  logger.info(s"未找到对应的用户信息，用户ID: ${userID}")
                  IO.pure(None)
              }
            } yield fullUserInfo
          }
  
        case None =>
          logger.info(s"Token ${userToken} 无效或未找到对应的记录")
          IO.pure(None)
      }
    } yield userInfo
  }
  
  def invalidateUserToken(userID: Int)(using PlanContext): IO[String] = {
  // val logger = LoggerFactory.getLogger("InvalidateUserTokenLogger")  // 同文后端处理: logger 统一
  
    for {
      // Step 1: 验证传入的userID是否有效
      _ <- IO(logger.info(s"[invalidateUserToken] 开始验证用户ID: ${userID}"))
      maybeUserInfo <- fetchUserInfoByID(userID)
      userInfo <- maybeUserInfo match {
        case Some(info) => 
          IO(logger.info(s"[invalidateUserToken] 用户信息验证成功: userID=${info.userID}, userName=${info.userName}")) *> IO.pure(info)
        case None => 
          val errorMessage = s"[invalidateUserToken] 用户ID [${userID}] 无效"
          IO(logger.error(errorMessage)) *> IO.raiseError(new IllegalArgumentException(errorMessage))
      }
  
      // Step 2: 使用户登录token失效
      _ <- IO(logger.info(s"[invalidateUserToken] 使用户登录token失效，用户ID: ${userID}"))
      invalidateSql <- IO {
        s"""
          UPDATE ${schemaName}.user_token_table
          SET token_status = 'INVALID'
          WHERE user_id = ?;
        """
      }
      invalidateParams <- IO {
        List(SqlParameter("Int", userID.toString))
      }
      updateResult <- writeDB(invalidateSql, invalidateParams)
      _ <- IO(logger.info(s"[invalidateUserToken] 登录token已失效，用户ID: ${userID}, 数据库更新结果: ${updateResult}"))
  
      // Step 3: 记录操作日志
      logDetails <- IO(s"用户信息更新，失效登录token")
      _ <- IO(logger.info(s"[invalidateUserToken] 准备记录操作日志: 操作=使登录token失效, 用户ID=${userID}, 详情=${logDetails}"))
      logResult <- recordUserAccountOperationLog("使登录token失效", userID, logDetails)
      _ <- IO(logger.info(s"[invalidateUserToken] 操作日志记录完成，结果: ${logResult}"))
  
      // Step 4: 返回操作结果
      result <- IO("登出成功!")
      _ <- IO(logger.info(s"[invalidateUserToken] 操作完成，结果: ${result}"))
    } yield result
  }
  
  def validateAdminToken(adminToken: String)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger("validateAdminToken")  // 同文后端处理: logger 统一
    logger.info(s"开始验证管理员Token，传入的adminToken: ${adminToken}")
  
    for {
      // Step 1: 获取用户信息
      userInfoOpt <- fetchUserInfoByToken(adminToken)
      _ <- IO(logger.info(s"fetchUserInfoByToken 返回结果: ${userInfoOpt.map(_.toString).getOrElse("None")}"))
  
      // Step 2: 验证用户信息是否存在及其角色
      isValid <- userInfoOpt match {
        case Some(userInfo) =>
          logger.info(s"获得了用户信息，用户角色是: ${userInfo.role.toString}")
          if (userInfo.role == UserRole.SuperAdmin) {
            logger.info("用户角色验证通过，该用户是超级管理员")
            IO.pure(true)
          } else {
            logger.info("用户角色验证失败，该用户不是超级管理员")
            IO.pure(false)
          }
        case None =>
          logger.info("未找到对应的用户信息或Token无效")
          IO.pure(false)
      }
    } yield isValid
  }
  
  def fetchUserInfoByID(userID: Int)(using PlanContext): IO[Option[UserInfo]] = {
  // val logger = LoggerFactory.getLogger(getClass)  // 同文后端处理: logger 统一
    logger.info(s"[fetchUserInfoByID] 根据用户ID[${userID}]查询用户信息")
  
    val sqlQuery = s"""
      SELECT user_id, user_name, account_name, password, role
      FROM ${schemaName}.user_account_table
      WHERE user_id = ?;
      """
    logger.info(s"[fetchUserInfoByID] 执行的SQL语句: ${sqlQuery}")
  
    readDBJsonOptional(
      sqlQuery,
      List(SqlParameter("Int", userID.toString))
    ).map {
      case Some(json) =>
        logger.info(s"[fetchUserInfoByID] 查询到的用户记录: ${json.noSpaces}")
        
        try {
          val userID = decodeField[Int](json, "user_id")
          val userName = decodeField[String](json, "user_name")
          val accountName = decodeField[String](json, "account_name")
          val password = decodeField[String](json, "password")
          val roleString = decodeField[String](json, "role")
          val userRole = UserRole.fromString(roleString)
  
          val userInfo = UserInfo(
            userID = userID,
            userName = userName,
            accountName = accountName,
            password = password,
            role = userRole
          )
  
          logger.info(s"[fetchUserInfoByID] 构造的UserInfo对象: ${userInfo}")
          Some(userInfo)
        } catch {
          case e: Exception =>
            logger.error(s"[fetchUserInfoByID] 构造UserInfo对象时出现错误: ${e.getMessage}")
            None
        }
  
      case None =>
        logger.info(s"[fetchUserInfoByID] 未找到用户信息，返回None")
        None
    }
  }
  
  def recordUserAccountOperationLog(
      operation: String,
      targetUserID: Int,
      details: String
  )(using PlanContext): IO[String] = {
    val validOperations = List("增加", "修改") // 预定义的操作类型
  // val logger = LoggerFactory.getLogger("recordUserAccountOperationLog")  // 同文后端处理: logger 统一
  
    // 步骤1: 参数校验
    for {
      _ <- if (operation.isEmpty || !validOperations.contains(operation)) {
        IO.raiseError(new IllegalArgumentException("操作类型无效！"))
      } else IO.unit
      _ <- if (targetUserID <= 0) {
        IO.raiseError(new IllegalArgumentException("目标用户ID无效！"))
      } else IO.unit
      _ <- if (details.isEmpty) {
        IO.raiseError(new IllegalArgumentException("操作细节信息不能为空！"))
      } else IO.unit
  
      _ <- IO(logger.info(s"开始记录用户账户操作日志，操作类型为：${operation}，目标用户ID为：${targetUserID}，细节为：${details}"))
  
      // 步骤2: 使用targetUserID获取用户安全信息
      maybeSafeUserInfo <- fetchSafeUserInfoByID(targetUserID)
      safeUserInfo <- maybeSafeUserInfo match {
        case Some(userInfo) => IO.pure(userInfo)
        case None => IO.raiseError(new IllegalArgumentException(s"目标用户ID [${targetUserID}] 对应的用户不存在，无法记录操作日志。"))
      }
  
      _ <- IO(logger.info(s"成功获取目标用户信息：用户ID=${safeUserInfo.userID}, 用户名=${safeUserInfo.userName}, 角色=${safeUserInfo.role.toString}"))
  
      // 步骤3: 构造日志数据
      timestamp <- IO(DateTime.now())
      logEntry <- IO {
        SystemLogEntry(
          logID = 0, // logID为自增主键，可以初始化为0，实际上由数据库生成
          timestamp = timestamp,
          userID = targetUserID,
          action = operation,
          details = details
        )
      }
  
      _ <- IO(logger.info(s"准备插入操作日志，内容为：时间=${timestamp}, 用户ID=${targetUserID}, 动作=${operation}, 细节=${details}"))
  
      // 步骤4: 插入数据库
      insertSQL <- IO {
        s"""
           INSERT INTO ${schemaName}.system_log_entry (timestamp, user_id, action, details)
           VALUES (?, ?, ?, ?)
        """.stripMargin
      }
      params <- IO {
        List(
          SqlParameter("DateTime", timestamp.getMillis.toString),
          SqlParameter("Int", targetUserID.toString),
          SqlParameter("String", operation),
          SqlParameter("String", details)
        )
      }
      dbResult <- writeDB(insertSQL, params)
  
      _ <- IO(logger.info(s"成功记录操作日志：${dbResult}"))
    } yield "日志记录成功！"
  }
  
  def validateAccountNameUniqueness(accountName: String)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger(getClass) // 声明 Logger  // 同文后端处理: logger 统一
  
    logger.info(s"[Validate Account Name Uniqueness] 开始验证账号名是否唯一: accountName=${accountName}")
  
    val sqlQuery =
      s"""
         SELECT account_name
         FROM ${schemaName}.user_account_table
         WHERE account_name = ?
      """
    val parameters = List(SqlParameter("String", accountName))
  
    logger.info(s"[Validate Account Name Uniqueness] 数据库查询 SQL: ${sqlQuery}")
    logger.info(s"[Validate Account Name Uniqueness] SQL 参数: ${parameters.map(_.value).mkString(", ")}")
  
    readDBJsonOptional(sqlQuery, parameters).flatMap {
      case Some(json) =>
        // 如果数据库中有记录，说明账号名已经存在，不唯一
        val existingAccountName = decodeField[String](json, "account_name")
        logger.info(s"[Validate Account Name Uniqueness] 账号名已存在: ${existingAccountName}")
        IO.pure(false)
      case None =>
        // 如果数据库中没有记录，说明账号名唯一
        logger.info(s"[Validate Account Name Uniqueness] 账号名是唯一的: ${accountName}")
        IO.pure(true)
    }
  }
  
  // 原代码问题: value SqlParameter is not a member of Common.DBAPI 是因为代码中自定义的 SqlParameter 类所在路径不正确. 使用 Common.Object.SqlParameter 可以解决编译错误问题.
}
