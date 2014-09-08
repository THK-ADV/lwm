package controllers

import akka.util.Timeout
import controllers.LabworkManagementController._
import models.{Students, UserForms, Users}
import play.api.mvc.{Action, Controller}
import play.libs.Akka
import utils.Security.Authentication
import utils.semantic.Resource

import scala.concurrent.Future


object UserManagement extends Controller with Authentication {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async { request =>
     for(users <- Users.all()) yield Ok(views.html.userManagement(users.toList))
    }
  }


  def userFirstTimeSelf = hasSession { session =>
    Action.async { implicit request =>
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors => {
          Future.successful(BadRequest(views.html.firstTimeInputUser(formWithErrors)))
        },
        user => {
          Users.create(user).map(_ => Redirect(routes.AdministrationDashboardController.dashboard()))

        }
      )
    }
  }

  def userPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { implicit request =>
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors => {
          for (all <- Users.all()) yield {
            BadRequest(views.html.userManagement(all.toList))
          }
        },
        user => {
          Users.create(user).map(_ => Redirect(routes.UserManagement.index()))
        }
      )
    }
  }

  def userRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async(parse.json) { implicit request =>
      val id = (request.body \ "id").as[String]
      Users.delete(Resource(id)).flatMap { deleted =>
        Users.all().map { all =>
          Redirect(routes.UserManagement.index())
        }
      }
    }
  }


}
