package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ Action, Controller }
import utils.semantic.{ StringLiteral, Resource, Individual }
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object StudentDashboardController extends Controller {

  def dashboard = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      for {
        student ← Students.get(session.user)
        courses ← Courses.all()
        labworks ← LabWorks.all()
        studentDegree = Individual(student).props(LWM.hasEnrollment).head.value
        mappedCourses = courses.map(c ⇒ (c, c.props.getOrElse(LWM.hasDegree, List(Resource(""))).head.value))
        properCourses = mappedCourses.filter(p ⇒ p._2 == studentDegree).map(_._1)
        mappedLabworks = labworks.map(l ⇒ (l, l.props.getOrElse(LWM.hasCourse, List(Resource(""))).head.value))
        properLabworks = mappedLabworks.filter(p ⇒ properCourses.map(_.uri.value).contains(p._2)).map(_._1)
        availableLabworks = properLabworks.filter(p ⇒ p.props.getOrElse(LWM.allowsApplications, List(StringLiteral(""))).head.value == "true")
      } yield {
        Ok(views.html.dashboard_student(availableLabworks, LabWorkForms.labWorkApplicationForm))
      }

    }
  }
}
