package controllers

import actors.SessionHandler
import actors.SessionHandler.{ Invalid, Valid }
import akka.actor.{ Actor, ActorRef, Props }
import akka.util.Timeout
import controllers.SessionManagement._
import models.{ Users, Students, UserForms }
import play.api.Logger
import play.api.libs.json.{ JsArray, JsString, Json, JsObject }
import play.api.mvc.{ Action, Controller, Security }
import play.libs.Akka
import utils.Security.Authentication
import utils.semantic.Resource

import scala.concurrent.Future

object StudentsManagement extends Controller with Authentication {

  import akka.pattern.ask

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import utils.Global._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      for {
        students ← Students.all()
      } yield {
        Ok(views.html.studentManagement(students.toList, UserForms.studentForm))
      }
    }
  }

  def studentFirstTimeSelf = hasSession { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          Future.successful(BadRequest(views.html.firstTimeInputStudents(formWithErrors)))
        },
        student ⇒ {
          Students.create(student).map(_ ⇒ Redirect(routes.StudentDashboardController.dashboard()))

        }
      )
    }
  }

  def studentPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for (all ← Students.all()) yield {
            BadRequest(views.html.studentManagement(all.toList, formWithErrors))
          }
        },
        student ⇒ {
          Students.create(student).map(_ ⇒ Redirect(routes.StudentsManagement.index()))
        }
      )
    }
  }

  def studentRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Students.delete(Resource(id)).flatMap { deleted ⇒
        Students.all().map { all ⇒
          Ok(views.html.studentManagement(all, UserForms.studentForm))
        }
      }
    }
  }

  def studentSuggestions = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val query = (request.body \ "query").as[String]
      val maxCountOpt = (request.body \ "maxCount").asOpt[String]
      val listFuture = maxCountOpt match {
        case None ⇒
          Students.search(query)
        case Some(c) ⇒
          Students.search(query, c.toInt)
      }
      listFuture.map { list ⇒
        val json = Json.obj(
          "query" -> JsString(query),
          "ids" -> JsArray(list.map(JsString)))

        render {
          case Accepts.Json() ⇒ Ok(json)
          case Accepts.JavaScript() => Ok(json)
        }
      }

    }
  }
}
