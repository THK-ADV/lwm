package controllers

import akka.actor.{Actor, Props}
import akka.util.Timeout
import controllers.Permissions.Role
import models.{LoginForms, Student, LoginData}
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

  def login() = Action.async { implicit request =>

    val loginData = LoginForms.studentForm.bindFromRequest.fold(
      formWithErrors => {
        None
      },
      login => {
        Some(login)
      }
    )

    loginData match{
      case None => Future.successful(Unauthorized)
      case Some(login) =>
        val timeoutFuture = Promise.timeout("Oops", 15.second)
        val authFuture = (sessionsHandler ? SessionHandler.AuthenticationRequest(login.user, login.password)).mapTo[Either[String, SessionHandler.Session]]

        Future.firstCompletedOf(Seq(authFuture, timeoutFuture)).map {
          case Left(message: String) =>
            Unauthorized(message)
          case Right(session: SessionHandler.Session) =>
           session.role match {
             case Permissions.AdminRole =>
               Redirect(routes.StudentsManagement.index()).withSession(
               Security.username -> login.user,
               "session" -> session.id,
               "expires" -> session.expirationDate.toString
             )
             case _ =>  Redirect(routes.Application.index()).withNewSession
           }
          case t: String => InternalServerError(t)
        }
    }


  }

  def logout() = Action { request =>
    request.session.get("session").map(sessionsHandler ! SessionHandler.LogoutRequest(_))
    Redirect(routes.Application.index()).withNewSession
  }
}


object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class LogoutRequest(sessionID: String)

  case class SessionRequest(id: String)

  case class Session(id: String, expirationDate: DateTime, user: String, groups: List[String], role: Role)

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor {

  import utils.LDAPAuthentication._
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
    case SessionRequest(id) =>
      sessions.find(_.id == id) map { session =>
        sender() ! session
      }
  }

  private def getRoles(user: String, group: List[String]): Role = {
    // TODO Get additional role information from user details
    if (group.contains("labor")) Permissions.AdminRole else Permissions.DefaultRole
  }

  private def createSessionID(user: String): Future[Session] = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")
    val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)
    val expirationDate = DateTime.now().plusHours(lifetime)
    val groups = groupMembership(user, bindHost, bindPort, GDN)

    groups map {
      case Left(error) =>
        val role = getRoles(user, Nil)
        Session(sessionID, expirationDate, user, Nil, role)
      case Right(gs) =>
        val role = getRoles(user, gs.toList)
        Session(sessionID, expirationDate, user, gs.toList, role)
    }
  }
}


object Permissions {

  sealed trait Permission

  object UserCreation extends Permission

  object UserDeletion extends Permission

  object UserModification extends Permission


  object UserInfoRead extends Permission

  object ScheduleRead extends Permission

  object ScheduleCreation extends Permission

  object ScheduleModification extends Permission

  object RoleModification extends Permission

  case class Role(permissions: Set[Permission]) {
    def +=(permission: Permission) = if (permissions.contains(Permissions.RoleModification)) Role(permissions + permission) else this

    def -=(permission: Permission) = if (permissions.contains(Permissions.RoleModification)) Role(permissions - permission) else this

    def contains(permission: Permission) = permissions.contains(permission)
  }

  val DefaultRole = Role(Set(ScheduleRead, UserInfoRead))

  val AdminRole = Role(Set(
    UserCreation, UserDeletion, UserModification,
    UserInfoRead,
    ScheduleRead, ScheduleCreation, ScheduleModification,
    RoleModification))

}