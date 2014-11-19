package controllers

import org.joda.time.{ LocalDate, DateTime }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication

import scala.concurrent.Future

object DefaultBounds {
  val min = "-1"
  val max = "7"
}
object AdministrationDashboardController extends Controller with Authentication {

  def dashboard(lowerBound: String, upperBound: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future.successful(Ok(views.html.dashboard_admin(LocalDate.now(), lowerBound.toInt, upperBound.toInt)))
    }
  }

}
