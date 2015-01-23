package controllers

import actors.TransactionsLoggerActor.Transaction
import controllers.AdministrationDashboardController._
import models._
import org.joda.time.LocalDateTime
import org.pegdown.PegDownProcessor
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ rdf, rdfs, lwm }
import utils.semantic._
import scala.concurrent.ExecutionContext.Implicits.global
import utils.Global._
import play.twirl.api.Html
import scala.concurrent.Future
import scala.util.control.NonFatal

object AssignmentManagementController extends Controller with Authentication {
  import Play.current
  val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          (for {
            assignments ← Assignments.all()
            courseResources ← Courses.all()
            courses = courseResources.map(c ⇒ Individual(c))
          } yield {
            Ok(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm))
          }).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def detailed(assignment: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          Courses.all().map(courses ⇒ Ok(views.html.assignment_detail(Individual(Resource(assignment)), courses.map(c ⇒ Individual(c)), AssignmentForms.assignmentForm, AssignmentForms.assignmentSolutionForm))).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
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
                courseResources ← Courses.all()
                courses = courseResources.map(c ⇒ Individual(c))
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors))
              }
            },
            a ⇒
              Assignments.create(Assignment(a.heading, a.description, a.text, a.goals, a.hints, a.topics.split(",").toList, a.courses)).map {
                assignment ⇒
                  system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(assignment.uri, s"New assignment created by ${session.user}")))
                  Redirect(routes.AssignmentManagementController.index())
              }
          ).recover {
              case NonFatal(e) ⇒
                InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
            }
      }
  }

  def assignmentRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          Assignments.delete(Resource(id)).map {
            assignment ⇒
              system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(assignment, s"Assignment deleted by ${session.user}.")))
              Redirect(routes.AssignmentManagementController.index())
          }.recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
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
                courseResources ← Courses.all()
                courses = courseResources.map(c ⇒ Individual(c))
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, formWithErrors))
              }
            },
            a ⇒ {
              system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(i.uri, s"Assignment modified by ${session.user}.")))
              val maybeId = i.props.get(rdfs.label)
              val maybeDesc = i.props.get(lwm.hasDescription)
              val maybeText = i.props.get(lwm.hasText)
              val maybeGoals = i.props.get(lwm.hasLearningGoals)
              val maybeHints = i.props.get(lwm.hasHints)
              val maybeTopics = i.props.get(lwm.hasTopic)
              val maybeCourses = i.props.get(lwm.hasCourse)
              for {
                label ← maybeId
                description ← maybeDesc
                text ← maybeText
                topics ← maybeTopics
                courses ← maybeCourses
                hints ← maybeHints
                goals ← maybeGoals
              } yield {
                label.headOption.map(head ⇒ i.update(rdfs.label, head, StringLiteral(a.heading)))
                description.headOption.map(head ⇒ i.update(lwm.hasDescription, head, StringLiteral(a.description)))
                text.headOption.map(head ⇒ i.update(lwm.hasText, head, StringLiteral(a.text)))
                hints.headOption.map(head ⇒ i.update(lwm.hasHints, head, StringLiteral(a.hints)))
                goals.headOption.map(head ⇒ i.update(lwm.hasLearningGoals, head, StringLiteral(a.goals)))
                topics.map(topic ⇒ i.remove(lwm.hasTopic, topic))
                if (a.courses.nonEmpty) courses.map(course ⇒ i.remove(lwm.hasCourse, course))
              }
              a.topics.split(",").map(t ⇒ i.add(lwm.hasTopic, StringLiteral(t)))
              a.courses.map(c ⇒ i.add(lwm.hasCourse, Resource(c)))
              Future(Redirect(routes.AssignmentManagementController.index()))
            }
          ).recover {
              case NonFatal(e) ⇒
                InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
            }
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
                courseResources ← Courses.all()
                courses = courseResources.map(c ⇒ Individual(c))
              } yield {
                BadRequest(views.html.assignmentManagement(assignments, courses, AssignmentForms.assignmentForm))
              }
            },
            a ⇒ {
              val query =
                s"""
                |select ?s (${lwm.hasAssignment} as ?p) (<$assignmentid> as ?o) where {
                | ?s ${rdf.typ} ${lwm.AssignmentSolution} .
                | ?s ${lwm.hasAssignment} <$assignmentid>
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
              for {
                s1 ← solution
                s2 ← s1
              } yield {
                system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(s2.uri, s"Assignment Solution created by ${session.user}.")))
                Redirect(routes.AssignmentManagementController.detailed(assignmentid))
              }
            }
          ).recover {
              case NonFatal(e) ⇒
                InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
            }
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
              system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), ModifyAction(i.uri, s"Assignment added to $labworkid by ${session.user}.")))
              i.add(lwm.hasAssignment, Resource(a.assignment))
              i.add(lwm.hasPreparationTime, StringLiteral(s"${
                a.preparationTime
              }"))
              Future(Redirect(routes.LabworkManagementController.edit(labworkid)))
            }
          ).recover {
              case NonFatal(e) ⇒
                InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
            }
      }
  }

  def bindRemoval(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val labworkid = (request.body \ "lId").as[String]
          val associationid = (request.body \ "aId").as[String]
          val i = Individual(Resource(associationid))
          i.props.get(lwm.hasAssignment).map { rList ⇒
            rList.map { a ⇒
              i.remove(lwm.hasAssignment, a)
            }
          }
          i.props.get(lwm.hasPreparationTime).map { rList ⇒
            rList.map { a ⇒
              i.remove(lwm.hasPreparationTime, a)
            }
          }
          system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), ModifyAction(i.uri, s"Assignment removed from $labworkid by ${session.user}.")))
          Future(Redirect(routes.LabworkManagementController.edit(labworkid))).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def exportAssignment(assignment: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          val i = Individual(Resource(assignment))
          val p = new PegDownProcessor()

          val text = p.markdownToHtml(i.props.getOrElse(lwm.hasText, List(StringLiteral(""))).head.value)
          val hints = p.markdownToHtml(i.props.getOrElse(lwm.hasHints, List(StringLiteral(""))).head.value)
          val goals = p.markdownToHtml(i.props.getOrElse(lwm.hasLearningGoals, List(StringLiteral(""))).head.value)
          val description = p.markdownToHtml(i.props.getOrElse(lwm.hasDescription, List(StringLiteral(""))).head.value)
          val label = i.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value
          val topics = i.props.getOrElse(lwm.hasTopic, List(StringLiteral(""))).head.value
          Future(Ok(views.html.assignment_export(label, Html.apply(description), Html.apply(text), Html.apply(hints), Html.apply(goals), topics))).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }

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
                |select ?s (${lwm.hasAssignment} as ?p) (<$assignmentid> as ?o) where {
                | ?s ${rdf.typ} ${lwm.AssignmentSolution} .
                | ?s ${lwm.hasAssignment} <$assignmentid>
                | }
                """.stripMargin

          val solutionFuture = sparqlExecutionContext.executeQuery(query).map {
            result ⇒
              SPARQLTools.statementsFromString(result).map(e ⇒ Individual(e.s))
          }

          solutionFuture.map { s ⇒
            val name = p.markdownToHtml(s.head.props.getOrElse(lwm.hasFileName, List(StringLiteral(""))).head.value)
            val text = p.markdownToHtml(s.head.props.getOrElse(lwm.hasText, List(StringLiteral(""))).head.value)
            val label = i.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value
            Ok(views.html.assignment_solution_export(label, Html(name), Html(text)))
          }.recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }

      }
  }
}