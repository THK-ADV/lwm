
package controllers

import actors.SessionHandler
import actors.SessionHandler.{Invalid, Valid}
import akka.util.Timeout
import models.UserForms
import play.api._
import play.api.mvc._
import play.libs.Akka
import utils.Security.Authentication

import scala.concurrent.Future

object Application extends Controller with Authentication{
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import akka.pattern.ask

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")


  override def onUnauthorized(request: RequestHeader): Result = Redirect(routes.Application.loginScreen())

  def index() = hasSession{ session =>
    Action{request =>
      session.role match{
        case Permissions.AdminRole => Redirect(routes.AdministrationDashboardController.dashboard())
        case Permissions.DefaultRole => Redirect(routes.StudentDashboardController.dashboard())
      }
    }
  }

  def loginScreen() = Action.async{ request =>
    val maybeToken = request.session.get("session")
    maybeToken match{
      case None =>
        Future.successful(Ok(views.html.login(UserForms.loginForm)))
      case Some(id) =>
        val responseFut = (sessionsHandler ? SessionHandler.SessionValidationRequest(id)).mapTo[SessionHandler.ValidationResponse]

        for {
          response <- responseFut
        } yield {
          response match {
            case Valid(session) =>
              Redirect(routes.Application.index())
            case Invalid =>
              Ok(views.html.login(UserForms.loginForm))
          }
        }
    }

  }

}
