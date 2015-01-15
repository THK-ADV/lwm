package controllers

import actors.TransactionsLoggerActor.Transaction
import models._
import org.joda.time.LocalDateTime
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ rdfs, lwm }
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.{ Future, ExecutionContext }

object DegreeManagementController extends Controller with Authentication {

  import ExecutionContext.Implicits.global
  import Play.current
  val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      for {
        degreeResources ← Degrees.all()
        degrees = degreeResources.map(d ⇒ Individual(d))
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
            degreeResources ← Degrees.all()
            degrees = degreeResources.map(d ⇒ Individual(d))
          } yield {
            BadRequest(views.html.degreeManagement(degrees.toList, formWithErrors))
          }
        },
        degree ⇒ {
          Degrees.create(degree).map { i ⇒
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(i, s"New Degree created by ${session.user}.")))
            Redirect(routes.DegreeManagementController.index())
          }
        }
      )
    }
  }

  def degreeRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Degrees.delete(Resource(id)).flatMap { deleted ⇒
        system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(deleted, s"Course deleted by ${session.user}.")))
        Degrees.all().map { all ⇒
          Ok(views.html.degreeManagement(all.map(e ⇒ Individual(e)), DegreeForms.degreeForm))
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
            degreeResources ← Degrees.all()
            degrees = degreeResources.map(d ⇒ Individual(d))
          } yield {
            BadRequest(views.html.degreeManagement(degrees.toList, formWithErrors))
          }
        },
        degree ⇒ {
          for {
            id ← i.props(lwm.hasId)
            name ← i.props(lwm.hasName)
          } yield {
            i.update(lwm.hasId, id, StringLiteral(degree.id))
            i.update(lwm.hasName, name, StringLiteral(degree.name))
            i.update(rdfs.label, name, StringLiteral(degree.name))
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(i.uri, s"Degree modified by ${session.user}.")))
          }
          Future.successful(Redirect(routes.DegreeManagementController.index()))
        }
      )
    }
  }
}
