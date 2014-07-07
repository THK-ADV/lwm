package util

import com.typesafe.config.ConfigFactory
import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}
import org.slf4j.LoggerFactory

import play.api.Play.current

import scala.concurrent.{ExecutionContext, Promise, Future}


object LDAPAuthentication  {
  import ExecutionContext.Implicits.global
  import scala.async.Async.{async, await}

  private val log = LoggerFactory.getLogger(getClass.getName)

  private val trustManager = new TrustAllTrustManager()
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()
  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  def authenticate(user: String, password: String):Either[String, Boolean] = {
    val config = ConfigFactory.load("application")
    val DN = config.getString("lwm.bindDN")
    val bindHost = config.getString("lwm.bindHost")
    val bindPort = config.getInt("lwm.bindPort")

    val bindDN = s"uid=$user, $DN"
    val bindRequest = new SimpleBindRequest(bindDN, password)

    bind[Boolean](bindHost, bindPort, "", "", ssl = true){connection =>
      try{
        val bindResult = connection.bind(bindRequest)
        if(bindResult.getResultCode == ResultCode.SUCCESS) Right(true) else Left("Invalid credentials")
      }catch{
        case e: LDAPException => Left(e.getMessage)
      } finally {
        connection.close()
      }
    }
  }

  def groupMembership(user: String): Set[String] = ???

  /**
   * Establishes a connection with the LDAP Server and runs an arbitrary function.
   * @param host the host of the LDAP server
   * @param port the port of the LDAP Server
   * @param dn 
   * @param password the password needed for the binding operation
   * @param ssl is it a secure connection?
   * @param f the function that is executed when the connection was established
   * @tparam A the return value when the function was successfully executed
   * @return the result of the function f
   */
  private def bind[A](host: String, port: Int, dn: String, password: String, ssl: Boolean = true)
             (f: LDAPConnection => Either[String, A]): Either[String, A] = {
    if (ssl) {
      val sslContext = sslUtil.createSSLContext("SSLv3")
      val connection = new LDAPConnection(sslContext.getSocketFactory)
      connection.setConnectionOptions(connectionOptions)
      connection.connect(host, port)
      f(connection)
    }else{
      val connection = new LDAPConnection(host, port)
      f(connection)
    }
  }
}
