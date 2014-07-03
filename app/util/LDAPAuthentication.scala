package util

import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}
import org.slf4j.LoggerFactory


object LDAPAuthentication  {

  private val log = LoggerFactory.getLogger(getClass.getName)

  private val trustManager = new TrustAllTrustManager()
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()
  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  def authenticate(user: String, password: String) = {
    val DN = "ou=people,ou=unix,dc=gm,dc=fh-koeln, dc=de" //play.Configuration.root().getString("lwm.bindDN")
    val bindHost = "advldapp.gm.fh-koeln.de" //play.Configuration.root().getString("lwm.bindHost")
    val bindPort = 6360 //play.Configuration.root().getString("lwm.bindPort")

    val bindDN = s"uid=$user, $DN"
    val bindRequest = new SimpleBindRequest(bindDN, password)

    bind(bindHost, bindPort, "", "", ssl = true){connection =>
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
