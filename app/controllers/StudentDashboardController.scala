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
      for {
        student ← Students.get(session.user)
        degree ← Students.getDegree(student)
      } yield {
        val pendingLabworkList = LabWorks.pendingApplications(student).map(Individual(_))
        val labworkList = LabWorks.openForDegree(degree).map(Individual(_))
        println(labworkList)
        val studentLabworkList = Students.labworksForStudent(student).toMap.keys.map(Individual(_)).toList
        Ok(views.html.dashboard_student(Individual(student), (labworkList diff pendingLabworkList) diff studentLabworkList, pendingLabworkList, Students.labworksForStudent(student).map(e ⇒ Individual(e._1) -> Seq(e._2)), LabworkApplications.Forms.labworkApplicationForm.fill(LabworkApplicationFormModel(session.user, "", Nil))))
      }
    }
  }

  def informationPage(id: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        val student = Individual(Resource(id))
        Degrees.all().map(d ⇒ Ok(views.html.dashboard_student_edit_details(student, d.map(e ⇒ Individual(e)), UserForms.studentForm))).recover { case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard()) }
    }
  }

  def assignmentsPage(labid: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒

    Action.async {
      implicit request ⇒

        Students.get(session.user).map { student ⇒
          val query =
            s"""
               |select ?s (${lwm.hasLabWork} as ?p) (<$labid> as ?o) where {
               | ?s ${rdf.typ} ${lwm.AssignmentAssociation} .
               | ?s ${lwm.hasOrderId} ?orderId .
               | ?s ${lwm.hasLabWork} <$labid> .
               | ?s ${lwm.isVisibleToStudents} "true"
               | } order by asc(?orderId)
              """.stripMargin

          val result = sparqlExecutionContext.executeQueryBlocking(query)
          val associations = SPARQLTools.statementsFromString(result).map(_.s)

          Ok(views.html.students_assignment_view(Individual(student), Individual(Resource(labid)), associations.toList))
        }
    }
  }
}
