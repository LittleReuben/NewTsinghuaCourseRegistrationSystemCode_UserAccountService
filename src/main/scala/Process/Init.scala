package Process

import Common.API.{API, PlanContext, TraceID}
import Common.DBAPI.{initSchema, writeDB, readDBInt}
import Common.ServiceUtils.schemaName
import Common.Object.SqlParameter
import Global.ServerConfig
import cats.effect.IO
import io.circe.generic.auto.*
import java.util.UUID
import java.security.MessageDigest
import Global.DBConfig
import Process.ProcessUtils.server2DB
import Global.GlobalVariables

object Init {
  def init(config: ServerConfig): IO[Unit] = {
    given PlanContext = PlanContext(traceID = TraceID(UUID.randomUUID().toString), 0)
    given DBConfig = server2DB(config)

    val program: IO[Unit] = for {
      _ <- IO(GlobalVariables.isTest=config.isTest)
      _ <- API.init(config.maximumClientConnection)
      _ <- Common.DBAPI.SwitchDataSourceMessage(projectName = Global.ServiceCenter.projectName).send
      _ <- initSchema(schemaName)
      /** 用户账户表，包含用户的基本账号信息
       * user_id: 用户的唯一ID，主键，自动递增
       * user_name: 用户名字
       * account_name: 用户账号名，唯一
       * password: 用户密码
       * role: 用户角色（枚举：SuperAdmin、Teacher、Student）
       */
      _ <- writeDB(
        s"""
        CREATE TABLE IF NOT EXISTS "${schemaName}"."user_account_table" (
            user_id SERIAL NOT NULL PRIMARY KEY,
            user_name TEXT NOT NULL,
            account_name TEXT NOT NULL,
            password TEXT NOT NULL,
            role TEXT NOT NULL
        );
        """,
        List()
      )

      // 🔽 新增：检查是否已有admin角色用户
      adminCount <- readDBInt(
        s"""
        SELECT COUNT(*) FROM "${schemaName}"."user_account_table"
        WHERE role = 'admin';
        """,
        List()
      )

      // 🔽 如果没有admin用户，插入默认管理员账户
      _ <- if adminCount == 0 then
        val rawPassword = "admin"
        val hashedPassword = MessageDigest.getInstance("SHA-256")
          .digest(rawPassword.getBytes("UTF-8"))
          .map("%02x".format(_)).mkString

        writeDB(
          s"""
          INSERT INTO "${schemaName}"."user_account_table"
            (user_name, account_name, password, role)
          VALUES (?, ?, ?, ?);
          """,
          List(
            SqlParameter("String", "admin"),
            SqlParameter("String", "Administrator"),
            SqlParameter("String", hashedPassword),
            SqlParameter("String", "admin")
          )
        )
      else IO.unit

    } yield ()

    program.handleErrorWith(err => IO {
      println("[Error] Process.Init.init 失败, 请检查 db-manager 是否启动及端口问题")
      err.printStackTrace()
    })
  }
}
