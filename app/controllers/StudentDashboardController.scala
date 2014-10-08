package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ DiscardingCookie, Action, Controller }
import utils.semantic.{ SPARQLTools, StringLiteral, Resource, Individual }
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

object StudentDashboardController extends Controller {
  import utils.Global._

  def dashboard = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      def availableLabworks(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${LWM.allowsApplications} as ?p) ?o where {
                     $student ${LWM.hasEnrollment} ?degree .
                     ?course ${LWM.hasDegree} ?degree .
                     ?o ${LWM.hasCourse} ?course .
                     ?o ${LWM.allowsApplications} "true" .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def pendingLabworkApplications(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${LWM.hasPendingApplication} as ?p) ?o where {
                     $student ${LWM.hasPendingApplication} ?application .
                     ?application ${LWM.hasLabWork} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def studentLabworks(student: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${LWM.hasLabWork} as ?p) ?o where {
                     $student ${LWM.memberOf} ?group .
                     ?group ${LWM.hasLabWork} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
        }
      }

      def studentLabworkGroup(student: Resource, labwork: Resource) = {
        val query =
          s"""
                   select ($student as ?s) (${LWM.memberOf} as ?p) ?o where {
                     $labwork ${LWM.hasGroup} ?group .
                     ?group ${LWM.hasMember} $student .
                     ?group ${RDFS.label} ?o .
                   }
                 """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
        }
      }

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
        Ok(views.html.dashboard_student(student, (labworkList diff pendingLabworkList) diff studentLabworkList, pendingLabworkList, labworkGroupAssocs.toList, LabworkApplications.Forms.labworkApplicationForm.fill(LabworkApplicationFormModel(session.user, "", Nil))))
      }).recover {
        case NonFatal(t) ⇒
          Ok(views.html.login(UserForms.loginForm)).withNewSession
      }

    }
  }
}
