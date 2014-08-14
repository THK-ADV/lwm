package controllers

import org.joda.time.DateTime
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication


object AdministrationDashboardController extends Controller with Authentication{

  def dashboard = hasPermissions(Permissions.AdminRole.permissions.toList : _*){session =>
    Action {
      Ok(views.html.dashboard_admin(List.empty, List.empty, DateTime.now()))
    }
  }
}
