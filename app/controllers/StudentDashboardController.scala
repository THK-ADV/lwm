package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ DiscardingCookie, Action, Controller }
import utils.semantic.{ SPARQLTools, StringLiteral, Resource, Individual }
import utils.semantic.Vocabulary.{ rdfs, lwm, rdf }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

object StudentDashboardController extends Controller {

  import utils.Global._

  def dashboard = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      def availableLabworks(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${lwm.allowsApplications} as ?p) ?o where {
                     $student ${lwm.hasEnrollment} ?degree .
                     ?course ${lwm.hasDegree} ?degree .
                     ?o ${lwm.hasCourse} ?course .
                     ?o ${lwm.allowsApplications} "true" .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def pendingLabworkApplications(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${lwm.hasPendingApplication} as ?p) ?o where {
                     $student ${lwm.hasPendingApplication} ?application .
                     ?application ${lwm.hasLabWork} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def studentLabworks(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${lwm.hasLabWork} as ?p) ?o where {
                     $student ${lwm.memberOf} ?group .
                     ?group ${lwm.hasLabWork} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def studentLabworkGroup(student: Resource, labwork: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${lwm.memberOf} as ?p) ?o where {
                     $labwork ${lwm.hasGroup} ?group .
                     ?group ${lwm.hasMember} $student .
                     ?group ${rdfs.label} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
        }
      }
      import utils.Global._
      (for {
        student ← Students.get(session.user)
        availableLabworks ← availableLabworks(student)
        labworkList = availableLabworks.map(r ⇒ Individual(r)).toList
        pendingLabworks ← pendingLabworkApplications(student)
        pendingLabworkList = pendingLabworks.map(r ⇒ Individual(r)).toList
        studentLabworks ← studentLabworks(student)
        studentLabworkList = studentLabworks.map(r ⇒ Individual(r)).toList
        labworkGroupAssocs ← Future.sequence(studentLabworks.map(r ⇒ studentLabworkGroup(student, r).map(l ⇒ Individual(r) -> l.map(_.decodedString))))
      } yield {
        Ok(views.html.dashboard_student(Individual(student), (labworkList diff pendingLabworkList) diff studentLabworkList, pendingLabworkList, labworkGroupAssocs.toList, LabworkApplications.Forms.labworkApplicationForm.fill(LabworkApplicationFormModel(session.user, "", Nil))))
      }).recover {
        case NonFatal(t) ⇒
          println(t)
          Ok(views.html.login(UserForms.loginForm)).withNewSession
      }
    }
  }

  def informationPage(id: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        val student = Individual(Resource(id))
        Degrees.all().map(d ⇒ Ok(views.html.dashboard_student_edit_details(student, d, UserForms.studentForm))).recover { case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard()) }
    }
  }

  def assignmentsPage(labid: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒

    Action.async {
      implicit request ⇒

        val studentFuture = for (s ← Students.get(session.user)(utils.Global.query)) yield s

        val query =
          s"""
         |select ?s (${lwm.hasLabWork} as ?p) (<$labid> as ?o) where {
         | ?s ${rdf.typ} ${lwm.AssignmentAssociation} .
         | ?s ${lwm.hasOrderId} ?orderId .
         | ?s ${lwm.hasLabWork} <$labid> .
         | ?s ${lwm.isVisibleToStudents} "true"
         | } order by asc(?orderId)
       """.stripMargin

        val assocFuture = sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.s)
        }
        (for {
          student ← studentFuture
          associations ← assocFuture
        } yield {
          Ok(views.html.students_assignment_view(Individual(student), Individual(Resource(labid)), associations.toList))
        }).recover {
          case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard())
        }
    }
  }
}
