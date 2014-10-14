package controllers

import models._
import org.pegdown.PegDownProcessor
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ SPARQLTools, StringLiteral, Resource, Individual }
import scala.concurrent.ExecutionContext.Implicits.global
import utils.Global._

import scala.concurrent.Future

/**
  * Created by root on 9/13/14.
  */
object AssignmentManagementController extends Controller with Authentication {

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          for {
            assignments ← Assignments.all()
            courses ← Courses.all()
          } yield {
            Ok(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm, AssignmentForms.assignmentSolutionForm))
          }
      }
  }

  def assignmentPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors, AssignmentForms.assignmentSolutionForm))
              }
            },
            a ⇒
              Assignments.create(Assignment(a.id, a.description, a.text, a.goals, a.hints, a.topics.split(",").toList, a.courses)).map { _ ⇒
                Redirect(routes.AssignmentManagementController.index())
              }
          )
      }
  }

  def assignmentRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          Assignments.delete(Resource(id)).map { _ ⇒
            Redirect(routes.AssignmentManagementController.index())
          }
      }
  }

  def assignmentEdit(assignmentid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          val i = Individual(Resource(assignmentid))
          AssignmentForms.assignmentForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors, AssignmentForms.assignmentSolutionForm))
              }
            },
            a ⇒ {
              val maybeId = i.props(RDFS.label)
              val maybeDesc = i.props(LWM.hasDescription)
              val maybeText = i.props(LWM.hasText)
              val maybeTopics = i.props(LWM.hasTopic)
              val maybeCourses = i.props(LWM.hasCourse)
              for {
                label ← maybeId
                description ← maybeDesc
                text ← maybeText
                topics ← maybeTopics
                courses ← maybeCourses
              } yield {
                i.update(RDFS.label, label, StringLiteral(a.id))
                i.update(LWM.hasDescription, description, StringLiteral(a.description))
                i.update(LWM.hasText, text, StringLiteral(a.text))
                i.remove(LWM.hasTopic, topics)
                i.remove(LWM.hasCourse, courses)
              }
              a.topics.split(",").map(t ⇒ i.add(LWM.hasTopic, StringLiteral(t)))
              a.courses.map(c ⇒ i.add(LWM.hasCourse, Resource(c)))
              Future.successful(Redirect(routes.AssignmentManagementController.index()))
            }
          )
      }
  }

  def assignmentSolutionPost(assignmentid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentSolutionForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm, formWithErrors))
              }
            },
            a ⇒
              AssignmentSolutions.create(AssignmentSolution(a.name, a.text, Resource(assignmentid))).map { _ ⇒
                Redirect(routes.AssignmentManagementController.index())
              }
          )
      }
  }

  def bindAssignment(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentAssociationForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                Redirect(routes.LabworkManagementController.edit(labworkid))
              }
            },
            a ⇒ {
              val i = Individual(Resource(associationid))
              i.add(LWM.hasAssignment, Resource(a.assignment))
              i.add(LWM.hasPreparationTime, StringLiteral(s"${a.preparationTime}"))
              Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
            }
          )
      }
  }

  def bindRemoval(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val labworkid = (request.body \ "lId").as[String]
          val associationid = (request.body \ "aId").as[String]
          val i = Individual(Resource(associationid))
          i.remove(LWM.hasAssignment, i.props.getOrElse(LWM.hasAssignment, List.empty[Resource]).head)
          Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
      }
  }

  def export(assignment: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      request ⇒
        val i = Individual(Resource(assignment))
        val p = new PegDownProcessor()
        println(assignment)
        val query =
          s"""
          |select ?s (${LWM.hasSemester} as ?p) ?o where {
          | ${i.uri} ${LWM.hasCourse} ?s .
          | ?labwork ${LWM.hasCourse} ?s .
          | ?labwork ${LWM.hasSemester} ?semester .
          | ?semester ${RDFS.label} ?o
          |}
        """.stripMargin

        val tupleFuture = sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o.value))
        }
        val text = p.markdownToHtml(i.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.value)
        val hints = p.markdownToHtml(i.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.value)
        val goals = p.markdownToHtml(i.props.getOrElse(LWM.hasLearningGoals, List(StringLiteral(""))).head.value)
        val description = p.markdownToHtml(i.props.getOrElse(LWM.hasDescription, List(StringLiteral(""))).head.value)
        val label = p.markdownToHtml(i.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value)

        tupleFuture.map(t ⇒ Ok(views.html.assignment_export(label, description, hints, goals, "", t.head)))

    }
  }
}