package controllers

import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.DateTime
import play.Configuration
import play.api.libs.concurrent.{Akka, Promise}
import play.api.mvc.{Action, Controller, Security}
import util.LDAPAuthentication._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Right, Left}
import play.api.Play.current


/**
 * Session Management.
 */
object SessionManagement extends Controller {

  import scala.concurrent.duration._
  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global


  private val config = play.Configuration.root()
  private implicit val timeout = Timeout(15.seconds)

  private val sessionsHandler = Akka.system.actorOf(SessionHandler.props(config), "sessions")

  /*
   * {
   *   user: "bla",
   *   password: "hallo123"
   * }
   */
  def login() = Action.async(parse.json) { request =>

    val user = (request.body \ "user").as[String]
    val password = (request.body \ "password").as[String]

    val timeoutFuture = Promise.timeout("Oops", 15.second)
    val authFuture = (sessionsHandler ? SessionHandler.AuthenticationRequest(user, password)).mapTo[Either[String, SessionHandler.Session]]


    Future.firstCompletedOf(Seq(authFuture, timeoutFuture)).map {
      case Left(message: String) =>
        Unauthorized(message)
      case Right(session: SessionHandler.Session) =>
        Ok("Login successful").withSession(
          Security.username -> "user",
          "session" -> session.id,
          "expires" -> session.expirationDate.toString
        )
      case t: String => InternalServerError(t)
    }
  }

  def logout() = Action { request =>
    request.session.get("session").map(sessionsHandler ! SessionHandler.LogoutRequest(_))

    NoContent.withNewSession
  }


}


object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class LogoutRequest(sessionID: String)

  case class Session(id: String, expirationDate: DateTime)

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor {

  import SessionHandler._

  private var sessions: Set[Session] = Set.empty

  val DN = config.getString("lwm.bindDN")
  val bindHost = config.getString("lwm.bindHost")
  val bindPort = config.getInt("lwm.bindPort")

  def receive: Receive = {
    case AuthenticationRequest(user, password) =>
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)
      val requester = sender()
      authFuture.map{
        case l @ Left(error) =>
          requester ! l
        case Right(success) =>
          val session =  createSessionID(user)
          sessions += session
          requester ! Right(session)
      }
    case LogoutRequest(sessionID) =>
      sessions.find(_.id == sessionID).map(sessions -= _)
  }

  private def createSessionID(user: String): Session = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")
    val lifetime = config.getInt("lwm.sessions.lifetime", 8)
    val expirationDate = DateTime.now().plusHours(lifetime)

    Session(sessionID, expirationDate)
  }
}