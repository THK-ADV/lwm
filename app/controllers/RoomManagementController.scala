package controllers

import controllers.LabworkManagementController._
import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.Resource

import scala.concurrent.Future

/**
 * Room Management:
 *
 */
object RoomManagementController extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { request =>
      for {
        rooms <- Rooms.all()
      } yield {
        Ok(views.html.room_management(rooms, Rooms.Forms.roomForm))
      }
    }
  }

  def roomPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { implicit request =>
      Rooms.Forms.roomForm.bindFromRequest.fold(
        formWithErrors => {
          for (all <- Rooms.all()) yield {
            BadRequest(views.html.room_management(all.toList, formWithErrors))
          }
        },
        room => {
          Rooms.create(Room(room.roomId, room.name)).map { i =>
            Redirect(routes.RoomManagementController.index())
          }
        }
      )
    }
  }

  def roomRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session =>
      Action.async(parse.json) {
        implicit request =>
          val id = (request.body \ "id").as[String]
          for (s<- Rooms.delete(Resource(id))) yield {
            Redirect(routes.RoomManagementController.index())
          }
      }
  }
}
