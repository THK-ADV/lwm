package util

import com.typesafe.config.ConfigFactory
import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The [[LDAPAuthentication]] object enables the user to communicate with an LDAP service.
 */
object LDAPAuthentication {

  private val log = LoggerFactory.getLogger(getClass.getName)

  private val trustManager = new TrustAllTrustManager()
  // Yes, it is actually a bad idea to trust every server but for now it's okay as we only use it with exactly one server in a private network.
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()
  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  /**
   * Tries to authenticate a user with the the LDAP service.
   * @param user the user
   * @param password the password for this user
   * @return either a boolean if the connection was successful or a String with the error message
   */
  def authenticate(user: String, password: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, Boolean]] = {
    val bindDN = s"uid=$user, $dn"
    val bindRequest = new SimpleBindRequest(bindDN, password)

    Future {
      bind[Boolean](bindHost, bindPort, dn, "", ssl = true) { connection =>
        try {
          val bindResult = connection.bind(bindRequest)
          if (bindResult.getResultCode == ResultCode.SUCCESS) Right(true) else Left("Invalid credentials")
        } catch {
          case e: LDAPException => Left(e.getMessage)
        } finally {
          connection.close()
        }
      }
    }
  }

  /**
   * Grabs all groups from LDAP.
   * @param user the user
   * @param bindHost the host
   * @param bindPort the port
   * @param dn the dn
   * @return Either an error message or a with the names of the groups
   */
  def groupMembership(user: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, Set[String]]] = Future {
    bind(bindHost, bindPort, dn, "") {
      connection =>
        try {
          import scala.collection.JavaConverters._
          val results = connection.search(dn, SearchScope.SUB, "(cn=*)", "*")
          Right(results.getSearchEntries.asScala.filter(_.getAttribute("memberUid").getValues.toList.contains(user)).map(_.getAttribute("cn").getValue).toSet)
        } catch {
          case e: LDAPException => Left(e.getMessage)
        } finally {
          connection.close()
        }
    }
  }

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
    } else {
      val connection = new LDAPConnection(host, port)
      f(connection)
    }
  }
}
