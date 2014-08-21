package controllers

import controllers.StudentsManagement._
import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication

import scala.concurrent.Future

/**
 * Created by rgiacinto on 20/08/14.
 */
object LabworkManagementController extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { request =>
      for {
        courses <- Courses.all()
        degrees <- Degrees.all()
        labworks <- LabWorks.all()
      } yield {
        Ok(views.html.labwork_management(labworks.toList, degrees.toList, courses.toList, LabWorkForms.labworkForm))
      }
    }
  }

  def labWorkPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { implicit request =>
      LabWorkForms.labworkForm.bindFromRequest.fold(
        formWithErrors => {
          for {
            labworks <- LabWorks.all()
            courses <- Courses.all()
            degrees <- Degrees.all()
          } yield {
            BadRequest(views.html.labwork_management(labworks.toList, degrees.toList, courses.toList, LabWorkForms.labworkForm))
          }
        },
        labwork => {
          LabWorks.create(labwork)
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        }
      )
    }
  }
}
