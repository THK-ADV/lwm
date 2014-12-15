package controllers

import actors.TransactionsLoggerActor.Transaction
import models._
import org.joda.time.LocalDateTime
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.{ Future, ExecutionContext }

/**
  * Created by rgiacinto on 21/08/14.
  */
object CourseManagementController extends Controller with Authentication {
  import ExecutionContext.Implicits.global
  import Play.current
  val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      for {
        courses ← Courses.all()
        degrees ← Degrees.all()
      } yield {
        Ok(views.html.courseManagement(degrees.toList, courses.toList, CourseForms.courseForm))
      }
    }
  }

  def coursePost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      CourseForms.courseForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            courses ← Courses.all()
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.courseManagement(degrees.toList, courses.toList, formWithErrors))
          }
        },
        course ⇒ {
          Courses.create(Course(course.name, course.id, Resource(course.degree))).map { c ⇒
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(c.uri, s"New Course created by ${session.user}.")))
            Redirect(routes.CourseManagementController.index())
          }
        }
      )
    }
  }

  def courseRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Courses.delete(Resource(id)).map { c ⇒
        system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), DeleteAction(c, s"Course deleted by ${session.user}.")))
        Redirect(routes.CourseManagementController.index())
      }
    }
  }

  def courseEdit(courseid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val i = Individual(Resource(courseid))
      CourseForms.courseForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            courses ← Courses.all()
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.courseManagement(degrees.toList, courses.toList, formWithErrors))
          }
        },
        course ⇒ {
          for {
            degree ← i.props(LWM.hasDegree)
            id ← i.props(LWM.hasId)
            name ← i.props(LWM.hasName)
          } yield {
            i.update(LWM.hasDegree, Resource(degree.value), Resource(course.degree))
            i.update(LWM.hasId, id, StringLiteral(course.id))
            i.update(LWM.hasName, name, StringLiteral(course.name))
            i.update(RDFS.label, name, StringLiteral(course.name))
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), ModifyAction(i.uri, s"Course modified by ${session.user}.")))
          }
          Future.successful(Redirect(routes.CourseManagementController.index()))
        }
      )
    }
  }
}
