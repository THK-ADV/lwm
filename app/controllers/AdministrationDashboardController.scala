package controllers

import play.api.mvc.{Action, Controller}


object AdministrationDashboardController extends Controller{
  def dashboard = Action {
    Ok("Administration Dashboard")
  }
}
