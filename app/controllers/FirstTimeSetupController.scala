package controllers

import models.{Student, User, UserForms}
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication


object FirstTimeSetupController extends Controller with Authentication{

  def setupStudent() =  hasSession{session =>
    Action { request =>
      val filledForm = UserForms.studentForm.fill(Student(session.user, "", "", "","", "", ""))
      Ok(views.html.firstTimeInputStudents(filledForm)).withSession(request.session + ("setup" -> "1"))
    }
  }

  def setupUser() = hasSession{session =>
    Action { request =>
      val filledForm = UserForms.userForm.fill(User(session.user, "", "", "",""))
     Ok(views.html.firstTimeInputUser(filledForm)).withSession(request.session + ("setup" -> "1"))
    }
  }
}
