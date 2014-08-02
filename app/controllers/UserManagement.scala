package controllers

import akka.util.Timeout
import models.{UserForms, Users}
import play.api.mvc.{Action, Controller}
import play.libs.Akka
import utils.Security.Authentication

import scala.concurrent.Future


object UserManagement extends Controller with Authentication {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)


  def userFirstTimeSelf = hasSession { session =>
    Action { implicit request =>
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"Errors: $formWithErrors")
          BadRequest(views.html.firstTimeInputUser(formWithErrors))
        },
        user => {
          Users.create(user)
          Redirect(routes.AdministrationDashboardController.dashboard())
        }
      )
    }
  }

  def userPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { implicit request =>
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors => {
          for (all <- Users.all()) yield {
            BadRequest(views.html.users(all.toList, formWithErrors))
          }
        },
        user => {
          Users.create(user)
          Future.successful(Redirect(routes.StudentsManagement.index()))
        }
      )
    }
  }
}
