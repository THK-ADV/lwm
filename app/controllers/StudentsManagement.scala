package controllers

import models.{ Degrees, Students, UserForms }
import play.api.libs.json.{ JsArray, JsString, Json }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Resource

object StudentsManagement extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global

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
          for {
            degrees ← Degrees.all()
          } yield BadRequest(views.html.firstTimeInputStudents(degrees, formWithErrors))
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
    Action.async { implicit request ⇒
      val query = request.queryString.getOrElse("q", List("")).head
      val max = request.queryString.getOrElse("max", List("0")).head
      Students.search(query, max.toInt).map { suggestions ⇒

        Ok(Json.obj(
          "query" -> JsString(query),
          "suggestions" -> JsArray(suggestions.map(s ⇒ Json.obj(
            "name" -> JsString(s._2),
            "id" -> JsString(s._1)
          )))
        ).toString())
      }

    }
  }
}
