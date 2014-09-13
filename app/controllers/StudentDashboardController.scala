package controllers

import controllers.AdministrationDashboardController._
import models.{ UserForms, Users, LabWorkForms }
import play.api.mvc.{ Action, Controller }

import scala.concurrent.Future

object StudentDashboardController extends Controller {
  def dashboard() = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      Future.successful(Ok(views.html.dashboard_student(Nil, LabWorkForms.labWorkApplicationForm)))
    }
  }
}
