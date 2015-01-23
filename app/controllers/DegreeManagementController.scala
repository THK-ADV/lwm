package controllers

import actors.TransactionsLoggerActor.Transaction
import controllers.AdministrationDashboardController._
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
import scala.util.control.NonFatal

object DegreeManagementController extends Controller with Authentication {

  import ExecutionContext.Implicits.global
  import Play.current
  val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      (for {
        degreeResources ← Degrees.all()
        degrees = degreeResources.map(d ⇒ Individual(d))
      } yield {
        Ok(views.html.degreeManagement(degrees.toList, DegreeForms.degreeForm))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
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
      ).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
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
      }.recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
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
      ).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }
}
