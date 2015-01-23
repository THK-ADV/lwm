package controllers

import controllers.SessionManagement._
import controllers.StudentInformationController._
import models._
import play.api.mvc.{ Action, Controller }
import utils.semantic.Vocabulary.{ lwm, rdf }
import utils.semantic.{ Individual, Resource }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

object StudentDashboardController extends Controller {

  import utils.Global._

  def dashboard = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      (for {
        student ← Students.get(session.user)
        degree ← Students.getDegree(student)
      } yield {
        val pendingLabworkList = LabWorks.pendingApplications(student).map(Individual(_))
        val labworkList = LabWorks.openForDegree(degree).map(Individual(_))
        println(labworkList)
        val studentLabworkList = Students.labworksForStudent(student).toMap.keys.map(Individual(_)).toList
        Ok(views.html.dashboard_student(Individual(student), (labworkList diff pendingLabworkList) diff studentLabworkList, pendingLabworkList, Students.labworksForStudent(student).map(e ⇒ Individual(e._1) -> Seq(e._2)), LabworkApplications.Forms.labworkApplicationForm.fill(LabworkApplicationFormModel(session.user, "", Nil))))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def informationPage(id: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        val student = Individual(Resource(id))
        Degrees.all().map(d ⇒ Ok(views.html.dashboard_student_edit_details(student, d.map(e ⇒ Individual(e)), UserForms.studentForm))).recover { case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard()) }.recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }

  def assignmentsPage(labid: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒

    Action.async {
      implicit request ⇒
        import utils.Implicits._
        Students.get(session.user).map { student ⇒
          val associations =
            s"""
               |select ?s (${lwm.hasLabWork} as ?p) (<$labid> as ?o) where {
                                                              |?s ${rdf.typ} ${lwm.AssignmentAssociation} .
                                                                                                           |?s ${lwm.hasOrderId} ?orderId .
                                                                                                                                  |?s ${lwm.hasLabWork} <$labid> .
                                                                                                                                                                 |?s ${lwm.isVisibleToStudents} "true"
                                                                                                                                                                                                 |} order by asc(?orderId)
              """.stripMargin.execSelect().map { solution ⇒
              Resource(solution.data("s").toString)
            }

          Ok(views.html.students_assignment_view(Individual(student), Individual(Resource(labid)), associations))
        }.recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }
}
