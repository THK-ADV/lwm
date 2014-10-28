package controllers

import models._
import org.pegdown.PegDownProcessor
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDF, RDFS, LWM }
import utils.semantic._
import scala.concurrent.ExecutionContext.Implicits.global
import utils.Global._
import play.twirl.api.Html
import scala.concurrent.Future
import scala.util.control.NonFatal

object AssignmentManagementController extends Controller with Authentication {

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          for {
            assignments ← Assignments.all()
            courses ← Courses.all()
          } yield {
            Ok(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm))
          }
      }
  }

  def detailed(assignment: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          Courses.all().map(courses ⇒ Ok(views.html.assignment_detail(Individual(Resource(assignment)), courses, AssignmentForms.assignmentForm, AssignmentForms.assignmentSolutionForm)))
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
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors))
              }
            },
            a ⇒
              Assignments.create(Assignment(a.heading, a.description, a.text, a.goals, a.hints, a.topics.split(",").toList, a.courses)).map {
                _ ⇒
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
          Assignments.delete(Resource(id)).map {
            _ ⇒
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
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors))
              }
            },
            a ⇒ {
              val maybeId = i.props.get(RDFS.label)
              val maybeDesc = i.props.get(LWM.hasDescription)
              val maybeText = i.props.get(LWM.hasText)
              val maybeGoals = i.props.get(LWM.hasLearningGoals)
              val maybeHints = i.props.get(LWM.hasHints)
              val maybeTopics = i.props.get(LWM.hasTopic)
              val maybeCourses = i.props.get(LWM.hasCourse)
              for {
                label ← maybeId
                description ← maybeDesc
                text ← maybeText
                topics ← maybeTopics
                courses ← maybeCourses
                hints ← maybeHints
                goals ← maybeGoals
              } yield {
                label.headOption.map(head ⇒ i.update(RDFS.label, head, StringLiteral(a.heading)))
                description.headOption.map(head ⇒ i.update(LWM.hasDescription, head, StringLiteral(a.description)))
                text.headOption.map(head ⇒ i.update(LWM.hasText, head, StringLiteral(a.text)))
                hints.headOption.map(head ⇒ i.update(LWM.hasHints, head, StringLiteral(a.hints)))
                goals.headOption.map(head ⇒ i.update(LWM.hasLearningGoals, head, StringLiteral(a.goals)))
                topics.map(topic ⇒ i.remove(LWM.hasTopic, topic))
                if (a.courses.nonEmpty) courses.map(course ⇒ i.remove(LWM.hasCourse, course))
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
                BadRequest(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm))
              }
            },
            a ⇒ {
              val query =
                s"""
                |select ?s (${LWM.hasAssignment} as ?p) (<$assignmentid> as ?o) where {
                | ?s ${RDF.typ} ${LWM.AssignmentSolution} .
                | ?s ${LWM.hasAssignment} <$assignmentid>
                | }
                """.stripMargin

              val solutionFuture = sparqlExecutionContext.executeQuery(query).map {
                result ⇒
                  SPARQLTools.statementsFromString(result).map(_.s)
              }

              val solution = {
                for (s ← solutionFuture) yield {
                  if (s.nonEmpty) AssignmentSolutions.delete(s.head)
                  AssignmentSolutions.create(AssignmentSolution(a.name, a.text, Resource(assignmentid)))
                }
              }
              (for {
                s1 ← solution
                s2 ← s1
              } yield {
                Redirect(routes.AssignmentManagementController.detailed(assignmentid))
              }).recover {
                case NonFatal(t) ⇒ Redirect(routes.AssignmentManagementController.index())
              }
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
              i.add(LWM.hasPreparationTime, StringLiteral(s"${
                a.preparationTime
              }"))
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
          i.props.get(LWM.hasAssignment).map { rList ⇒
            rList.map { a ⇒
              i.remove(LWM.hasAssignment, a)
            }
          }
          i.props.get(LWM.hasPreparationTime).map { rList ⇒
            rList.map { a ⇒
              i.remove(LWM.hasPreparationTime, a)
            }
          }
          Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
      }
  }

  def exportAssignment(assignment: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          val i = Individual(Resource(assignment))
          val p = new PegDownProcessor()

          val text = p.markdownToHtml(i.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.value)
          val hints = p.markdownToHtml(i.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.value)
          val goals = p.markdownToHtml(i.props.getOrElse(LWM.hasLearningGoals, List(StringLiteral(""))).head.value)
          val description = p.markdownToHtml(i.props.getOrElse(LWM.hasDescription, List(StringLiteral(""))).head.value)
          val label = i.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value
          val topics = i.props.getOrElse(LWM.hasTopic, List(StringLiteral(""))).head.value
          Future.successful(Ok(views.html.assignment_export(label, Html.apply(description), Html.apply(text), Html.apply(hints), Html.apply(goals), topics)))

      }
  }

  def exportSolution(assignmentid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          val i = Individual(Resource(assignmentid))
          val p = new PegDownProcessor()

          val query =
            s"""
                |select ?s (${LWM.hasAssignment} as ?p) (<$assignmentid> as ?o) where {
                | ?s ${RDF.typ} ${LWM.AssignmentSolution} .
                | ?s ${LWM.hasAssignment} <$assignmentid>
                | }
                """.stripMargin

          val solutionFuture = sparqlExecutionContext.executeQuery(query).map {
            result ⇒
              SPARQLTools.statementsFromString(result).map(e ⇒ Individual(e.s))
          }

          solutionFuture.map { s ⇒
            val name = p.markdownToHtml(s.head.props.getOrElse(LWM.hasFileName, List(StringLiteral(""))).head.value)
            val text = p.markdownToHtml(s.head.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.value)
            val label = i.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value
            Ok(views.html.assignment_solution_export(label, Html(name), Html(text)))
          }

      }
  }
}