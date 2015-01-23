package controllers

import akka.actor.ActorSystem
import controllers.AdministrationDashboardController._
import controllers.LabworkManagementController._
import models._
import play.api.Play
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.Vocabulary.{ rdfs, lwm }
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.Future
import utils.QueryHost

import scala.util.control.NonFatal

/**
  * Room Management:
  *
  */
object RoomManagementController extends Controller with Authentication with TransactionSupport {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import Play.current
  override def system: ActorSystem = Akka.system()

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      (for {
        rooms ← Rooms.all()
      } yield {
        Ok(views.html.room_management(rooms.map(e ⇒ Individual(e)), Rooms.Forms.roomForm))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def roomPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Rooms.Forms.roomForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for (all ← Rooms.all()) yield {
            BadRequest(views.html.room_management(all.map(e ⇒ Individual(e)).toList, formWithErrors))
          }
        },
        room ⇒ {
          Rooms.create(Room(room.roomId, room.name)).map { i ⇒
            createTransaction(session.user, i, s"Room $i created by ${session.user}")
            Redirect(routes.RoomManagementController.index())
          }
        }
      ).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }

  def roomRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          Rooms.delete(Resource(id)).map { r ⇒
            deleteTransaction(session.user, r, s"Room $r removed by ${session.user}")
            Redirect(routes.RoomManagementController.index())
          }.recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def roomEdit(roomId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        val i = Individual(Resource(roomId))
        Rooms.Forms.roomForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            for (all ← Rooms.all()) yield {
              BadRequest(views.html.room_management(all.map(e ⇒ Individual(e)).toList, formWithErrors))
            }
          },
          room ⇒ {
            for {
              id ← i.props(lwm.hasRoomId)
              name ← i.props(lwm.hasName)
            } yield {
              i.update(lwm.hasRoomId, id, StringLiteral(room.roomId))
              i.update(lwm.hasName, name, StringLiteral(room.name))
              i.update(rdfs.label, name, StringLiteral(room.name))
              modifyTransaction(session.user, i.uri, s"Room ${i.uri} modified by ${session.user}")
            }
            Future(Redirect(routes.RoomManagementController.index()))
          }
        ).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

}
