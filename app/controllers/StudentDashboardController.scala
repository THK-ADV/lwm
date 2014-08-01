package controllers

import play.api.mvc.{Action, Controller}


object StudentDashboardController extends Controller{
  def dashboard = Action {
    Ok("Student Dashboard")
  }
}
