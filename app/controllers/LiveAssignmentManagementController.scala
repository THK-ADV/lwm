package controllers

import java.net.URLDecoder

import models.{ LiveAssignment, LiveAssignments }
import org.pegdown.PegDownProcessor
import play.api.mvc.{ Action, Controller, Result }
import play.twirl.api.Html
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }
import scala.util.Random

object LiveAssignmentManagementController extends Controller with Authentication {
  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        LiveAssignments.tags().map { las ⇒
          Ok(views.html.liveAssignments.tagIndex(las, LiveAssignments.Forms.addForm))
        }
      }
  }

  def tagIndex(tag: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        LiveAssignments.all(tag).map { las ⇒
          Ok(views.html.liveAssignments.index(las, LiveAssignments.Forms.addForm))
        }
      }
  }

  def assignmentAddition() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        val p = Promise[Result]()
        LiveAssignments.Forms.addForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            p.success(Redirect(routes.LiveAssignmentManagementController.index()))
          },
          liveAssignment ⇒ {
            LiveAssignments.create(LiveAssignment(liveAssignment.title, liveAssignment.assignment, liveAssignment.example, liveAssignment.topics.split(",").toList)).map { i ⇒
              p.success(Redirect(routes.LiveAssignmentManagementController.index()))
            }
          }
        )

        p.future
      }
  }

  def assignmentRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒
        val maybeId = (request.body \ "id").asOpt[String]
        val p = Promise[Result]()
        maybeId match {
          case None ⇒ p.success(Redirect(routes.LiveAssignmentManagementController.index()))
          case Some(id) ⇒
            LiveAssignments.delete(Resource(id)).map(r ⇒ p.success(Redirect(routes.LiveAssignmentManagementController.index())))
        }

        p.future
      }
  }

  def show(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action { implicit request ⇒
        import utils.Global._
        val a = Individual(Resource(id))
        val p = new PegDownProcessor()

        val text = a.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.toString
        val title = a.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.toString
        val example = a.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.toString
        Ok(views.html.liveAssignments.show(Html(p.markdownToHtml(title)), Html(p.markdownToHtml(text)), Html(p.markdownToHtml(example))))
      }
  }

  def showRandom(tag: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        LiveAssignments.all(tag).map { asses ⇒
          val ass = asses.toArray
          val a = ass(Random.nextInt(asses.size))
          val p = new PegDownProcessor()

          val text = a.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.toString
          val title = a.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.toString
          val example = a.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.toString
          Ok(views.html.liveAssignments.show(Html(p.markdownToHtml(title)), Html(p.markdownToHtml(text)), Html(p.markdownToHtml(example))))
        }
      }
  }
  def assignmentEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      LiveAssignments.Forms.addForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          Future.successful(Redirect(routes.LiveAssignmentManagementController.index()))
        },
        liveAssignment ⇒ {
          val ass = Individual(Resource(id))

          val title = ass.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value
          val text = ass.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.value
          val hints = ass.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.value
          val topics = ass.props.getOrElse(LWM.hasTopic, List(StringLiteral(""))).map {
            topic ⇒
              ass.remove(LWM.hasTopic, topic)
          }

          ass.update(RDFS.label, StringLiteral(title), StringLiteral(liveAssignment.title))
          ass.update(LWM.hasText, StringLiteral(text), StringLiteral(liveAssignment.assignment))
          ass.update(LWM.hasHints, StringLiteral(hints), StringLiteral(liveAssignment.example))
          liveAssignment.topics.split(",").map { topic ⇒
            ass.add(LWM.hasTopic, StringLiteral(topic.trim))
          }
          Future.successful(Redirect(routes.LiveAssignmentManagementController.index()))
        }
      )
    }
  }

}
