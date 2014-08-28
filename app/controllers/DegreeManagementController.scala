package controllers

import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.Resource

import scala.concurrent.{Future, ExecutionContext}


object DegreeManagementController extends Controller with Authentication{
  import ExecutionContext.Implicits.global

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async { request =>
      for{
        degrees <- Degrees.all()
      } yield{
        Ok(views.html.degreeManagement(degrees.toList, DegreeForms.degreeForm))
      }
    }
  }

  def degreePost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { implicit request =>
      DegreeForms.degreeForm.bindFromRequest.fold(
        formWithErrors => {
          for {
            degrees <- Degrees.all()
          } yield {
            BadRequest(views.html.degreeManagement(degrees.toList, formWithErrors))
          }
        },
        degree => {
          Degrees.create(degree)
          Future.successful(Redirect(routes.DegreeManagementController.index()))
        }
      )
    }
  }

  def degreeRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async(parse.json) { implicit request =>
      for{
        degrees <- Degrees.all()
      } yield{
        val id = (request.body \ "id").as[String]
        Degrees.delete(Resource(id))
        Ok(views.html.degreeManagement(degrees.toList, DegreeForms.degreeForm))
      }
    }
  }
}
