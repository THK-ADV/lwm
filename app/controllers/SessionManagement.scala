package controllers

import actors.{ UserCountSocketActor, SessionHandler }
import akka.util.Timeout
import models.{ Students, UserForms }
import play.api.libs.concurrent.{ Promise ⇒ PlayPromise }
import play.api.libs.json.JsValue
import play.api.mvc._
import play.libs.Akka
import play.api.Play.current

import scala.concurrent.{ Promise, Await, Future }

/**
  * Session Management.
  */
object SessionManagement extends Controller {

  import akka.pattern.ask

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(60.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

  def login() = Action.async { implicit request ⇒

    val loginData = UserForms.loginForm.bindFromRequest.fold(
      formWithErrors ⇒ {
        println(formWithErrors)
        None
      },
      login ⇒ {
        Some(login)
      }
    )

    loginData match {
      case None ⇒ Future.successful(Unauthorized)
      case Some(login) ⇒
        val timeoutFuture = PlayPromise.timeout("No response from IDM", 45.second)
        val authFuture = (sessionsHandler ? SessionHandler.AuthenticationRequest(login.user.toLowerCase, login.password)).mapTo[Either[String, SessionHandler.Session]]

        Future.firstCompletedOf(Seq(authFuture, timeoutFuture)).map {
          case Left(message: String) ⇒
            Redirect(routes.Application.index()).withNewSession
          case Right(session: SessionHandler.Session) ⇒
            val firstTime = Await.result(firstTimeCheck(session.user), atMost = 20.seconds)
            session.role match {
              case Permissions.AdminRole ⇒
                if (firstTime) {
                  Redirect(routes.FirstTimeSetupController.setupUser()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id
                  )
                } else {
                  Redirect(routes.AdministrationDashboardController.dashboard("7")).withSession(
                    Security.username -> login.user,
                    "session" -> session.id
                  )
                }
              case _ ⇒
                if (firstTime) {
                  Redirect(routes.FirstTimeSetupController.setupStudent()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id
                  )
                } else {
                  Redirect(routes.StudentDashboardController.dashboard()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id
                  )
                }
            }
          case t: String ⇒ Ok(views.html.error(t))
        }
    }

  }

  def firstTimeCheck(user: String): Future[Boolean] = Students.exists(user).map { b ⇒ !b }

  def logout() = Action { request ⇒
    import play.api.libs.json._
    request.session.get("session").map(sessionsHandler ! SessionHandler.LogoutRequest(_))
    val json = Json.toJson(Map(
      "url" -> routes.Application.index().url
    ))

    Ok(json)
  }

  def userCountSocket = WebSocket.tryAcceptWithActor[JsValue, JsValue] { request ⇒
    val p = Promise[Either[Result, WebSocket.HandlerProps]]()
    request.session.get("session").map { token ⇒
      (sessionsHandler ? SessionHandler.SessionValidationRequest(token)).map {
        case SessionHandler.Valid(session) ⇒
          if (session.role.contains(Permissions.ScheduleAssociationModification)) {
            p.success(Right(UserCountSocketActor.props))
          } else {
            p.success(Left(Unauthorized("Insufficient Access Rights")))
          }
        case SessionHandler.Invalid ⇒
          p.success(Left(Unauthorized("Invalid Session")))
      }
    }

    p.future
  }
}

object Permissions {

  sealed trait Permission

  case object UserCreation extends Permission

  case object UserDeletion extends Permission

  case object UserModification extends Permission

  case object UserInfoRead extends Permission

  case object ScheduleRead extends Permission

  case object ScheduleCreation extends Permission

  case object ScheduleModification extends Permission

  case object RoleModification extends Permission

  case object ScheduleAssociationModification extends Permission

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
    ScheduleAssociationModification,
    RoleModification))

}