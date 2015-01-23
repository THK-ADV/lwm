package controllers

import org.joda.time.LocalDate
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication

import scala.concurrent.Future
import scala.util.control.NonFatal

object DefaultBounds {
  val min = "-1"
  val max = "7"
}

object AdministrationDashboardController extends Controller with Authentication {
  import scala.concurrent.ExecutionContext.Implicits.global

  def dashboard(lowerBound: String, upperBound: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future(Ok(views.html.dashboard_admin(LocalDate.now(), lowerBound.toInt, upperBound.toInt))).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

}
