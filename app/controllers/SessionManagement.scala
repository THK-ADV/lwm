package controllers

import akka.actor.{Actor, Props}
import akka.util.Timeout
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.DateTime
import play.api.Configuration
import play.api.libs.concurrent.Promise
import play.api.mvc.{Security, Action, Controller}
import play.libs.Akka

import scala.concurrent.Future


/**
 * Session Management.
 */
object SessionManagement extends Controller {

  import scala.concurrent.duration._
  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global
  import play.api.Play.current


  private implicit val timeout = Timeout(15.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

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

  case class SessionRequest(user: String)

  case class Session(id: String, expirationDate: DateTime, user: String, groups: List[String])

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor {

  import util.LDAPAuthentication._
  import SessionHandler._
  import scala.concurrent.ExecutionContext.Implicits.global

  private var sessions: Set[Session] = Set.empty

  val DN = config.getString("lwm.bindDN").get
  val GDN = config.getString("lwm.groupDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get

  def receive: Receive = {
    case AuthenticationRequest(user, password) =>
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)

      val requester = sender()
      authFuture.map {
        case l@Left(error) =>
          requester ! l
        case Right(success) =>
          val sessionFuture = createSessionID(user)
          sessionFuture map { session =>
            sessions += session
            requester ! Right(session)
          }
      }
    case LogoutRequest(sessionID) =>
      sessions.find(_.id == sessionID).map(sessions -= _)
    case SessionRequest(user) =>
      sessions.find(_.user == user) map { session =>
        sender() ! session
      }
  }

  private def createSessionID(user: String): Future[Session] = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")
    val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)
    val expirationDate = DateTime.now().plusHours(lifetime)
    val groups = groupMembership(user, bindHost, bindPort, GDN)

    groups map {
      case Left(error) => Session(sessionID, expirationDate, user, Nil)
      case Right(gs) => Session(sessionID, expirationDate, user, gs.toList)
    }
  }
}


object Permissions {


  sealed trait Permission

  trait UserCreation extends Permission

  trait UserInfoRead extends Permission

  trait ScheduleRead extends Permission

  trait ScheduleCreation extends Permission

  trait ScheduleModification extends Permission

  trait PermissionModification extends Permission

  class DefaultPermissions extends Permission with ScheduleRead with UserInfoRead

  class AdminPermissions extends DefaultPermissions
  with UserCreation
  with ScheduleCreation
  with PermissionModification
  with ScheduleModification

}