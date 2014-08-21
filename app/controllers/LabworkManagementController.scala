package controllers

import controllers.StudentsManagement._
import models.{UserForms, Students}
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication

import scala.concurrent.Future

/**
 * Created by rgiacinto on 20/08/14.
 */
object LabworkManagementController extends Controller with Authentication{

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async { request =>
      Future.successful(Ok(views.html.labwork_management()))
    }
  }
}
