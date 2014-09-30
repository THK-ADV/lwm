package controllers

import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.{ Future, ExecutionContext }

object DegreeManagementController extends Controller with Authentication {

  import ExecutionContext.Implicits.global

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      for {
        degrees ← Degrees.all()
      } yield {
        Ok(views.html.degreeManagement(degrees.toList, DegreeForms.degreeForm))
      }
    }
  }

  def degreePost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      DegreeForms.degreeForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.degreeManagement(degrees.toList, formWithErrors))
          }
        },
        degree ⇒ {
          Degrees.create(degree).map { _ ⇒ Redirect(routes.DegreeManagementController.index()) }
        }
      )
    }
  }

  def degreeRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Degrees.delete(Resource(id)).flatMap { deleted ⇒
        Degrees.all().map { all ⇒
          Ok(views.html.degreeManagement(all, DegreeForms.degreeForm))
        }
      }
    }
  }

  def degreeEdit(degreeid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val i = Individual(Resource(degreeid))
      DegreeForms.degreeForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.degreeManagement(degrees.toList, formWithErrors))
          }
        },
        degree ⇒ {
          val maybeId = i.props(LWM.hasId)
          val maybeName = i.props(LWM.hasName)
          for {
            id ← maybeId
            name ← maybeName
          } yield {
            i.update(LWM.hasId, id, StringLiteral(degree.id))
            i.update(LWM.hasName, name, StringLiteral(degree.name))
          }
          Future.successful(Redirect(routes.DegreeManagementController.index()))
        }
      )
    }
  }
}
