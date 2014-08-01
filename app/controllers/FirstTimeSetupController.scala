package controllers

import models.UserForms
import play.api.mvc.{Action, Controller}


object FirstTimeSetupController extends Controller{
  def setupStudent() = Action { request =>
    request.session.get("session") match{
      case None => Redirect(routes.Application.index())
      case Some(id) =>

        Ok(views.html.firstTimeInputStudents(UserForms.studentForm)).withSession(request.session + ("setup" -> "1"))
    }
  }

  def setupUser() = Action {request =>
    request.session.get("session") match{
      case None => Redirect(routes.Application.index())
      case Some(id) => Ok(views.html.firstTimeInputUser())
    }
  }
}
