package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ Action, Controller }
import utils.semantic.{ SPARQLTools, StringLiteral, Resource, Individual }
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

      for {
        student ← Students.get(session.user)
        availableLabworks ← availableLabworks(student)
        labworkList = availableLabworks.map(r ⇒ Individual(r)).toList
      } yield {
        Ok(views.html.dashboard_student(labworkList, LabworkApplications.Forms.labworkApplicationForm.fill(LabworkApplicationFormModel(session.user, "", Nil))))
      }

    }
  }
}
