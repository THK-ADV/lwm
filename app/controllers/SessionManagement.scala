package controllers

import actors.SessionHandler
import akka.util.Timeout
import models.{ Students, UserForms }
import play.api.libs.concurrent.Promise
import play.api.mvc.{ Action, Controller, Security }
import play.libs.Akka

import scala.concurrent.{ Await, Future }

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
        None
      },
      login ⇒ {
        Some(login)
      }
    )

    loginData match {
      case None ⇒ Future.successful(Unauthorized)
      case Some(login) ⇒
        val timeoutFuture = Promise.timeout("No response from IDM", 45.second)
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
                    "session" -> session.id,
                    "expires" -> session.expirationDate.toString
                  )
                } else {
                  Redirect(routes.AdministrationDashboardController.dashboard()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id,
                    "expires" -> session.expirationDate.toString
                  )
                }
              case _ ⇒
                if (firstTime) {
                  Redirect(routes.FirstTimeSetupController.setupStudent()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id,
                    "expires" -> session.expirationDate.toString
                  )
                } else {
                  Redirect(routes.StudentDashboardController.dashboard()).withSession(
                    Security.username -> login.user,
                    "session" -> session.id,
                    "expires" -> session.expirationDate.toString
                  )
                }
            }
          case t: String ⇒ InternalServerError(t)
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