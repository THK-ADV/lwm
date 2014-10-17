package controllers

import org.joda.time.LocalDate
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Resource

import scala.concurrent.Future

object SupervisionController extends Controller with Authentication {

  def supervise(id: String, date: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      Future.successful(Ok(views.html.supervision(Resource(id), LocalDate.parse(date))))
    }
  }
}