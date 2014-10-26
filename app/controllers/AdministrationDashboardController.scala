package controllers

import javax.mail.Message

import actors.EmailHandler
import actors.EmailHandler.EmailResponse
import akka.util.Timeout
import org.joda.time.{ LocalDate, DateTime }
import play.api.libs.concurrent.Promise
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication

import scala.concurrent.Future

object AdministrationDashboardController extends Controller with Authentication {

  def dashboard = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future.successful(Ok(views.html.dashboard_admin(LocalDate.now())))
    }
  }
}
