package controllers

import controllers.AdministrationDashboardController._
import play.api.mvc.{Action, Controller}


object StudentDashboardController extends Controller{
  def dashboard() =  hasPermissions(Permissions.DefaultRole.permissions.toList : _*){session =>
    Action {
      Ok(views.html.dashboardStudent())
    }
  }
}
