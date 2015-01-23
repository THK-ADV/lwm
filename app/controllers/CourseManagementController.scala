package controllers

import actors.TransactionsLoggerActor.Transaction
import controllers.AdministrationDashboardController._
import models._
import org.joda.time.LocalDateTime
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ rdfs, lwm }
import utils.semantic.{ StringLiteral, Individual, Resource }
import utils.Global._
import scala.concurrent.{ Future, ExecutionContext }
import scala.util.control.NonFatal

/**
  * Created by rgiacinto on 21/08/14.
  */
object CourseManagementController extends Controller with Authentication {
  import ExecutionContext.Implicits.global
  import Play.current
  val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      (for {
        courseResources ← Courses.all()
        degreeResources ← Degrees.all()
        courses = courseResources.map(c ⇒ Individual(c))
        degrees = degreeResources.map(d ⇒ Individual(d))
      } yield {
        Ok(views.html.courseManagement(degrees.toList, courses.toList, CourseForms.courseForm))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def coursePost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      CourseForms.courseForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            courseResources ← Courses.all()
            degreeResources ← Degrees.all()
            courses = courseResources.map(c ⇒ Individual(c))
            degrees = degreeResources.map(d ⇒ Individual(d))
          } yield {
            BadRequest(views.html.courseManagement(degrees.toList, courses.toList, formWithErrors))
          }
        },
        course ⇒ {
          Courses.create(Course(course.name, course.id, Resource(course.degree))).map { c ⇒
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), CreateAction(c, s"New Course created by ${session.user}.")))
            Redirect(routes.CourseManagementController.index())
          }
        }
      ).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }

  def courseRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Courses.delete(Resource(id)).map { c ⇒
        system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), DeleteAction(c, s"Course deleted by ${session.user}.")))
        Redirect(routes.CourseManagementController.index())
      }.recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def courseEdit(courseid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val i = Individual(Resource(courseid))
      CourseForms.courseForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            courseResources ← Courses.all()
            degreeResources ← Degrees.all()
            courses = courseResources.map(c ⇒ Individual(c))
            degrees = degreeResources.map(d ⇒ Individual(d))
          } yield {
            BadRequest(views.html.courseManagement(degrees.toList, courses.toList, formWithErrors))
          }
        },
        course ⇒ {
          for {
            degree ← i.props(lwm.hasDegree)
            id ← i.props(lwm.hasId)
            name ← i.props(lwm.hasName)
          } yield {
            i.update(lwm.hasDegree, Resource(degree.value), Resource(course.degree))
            i.update(lwm.hasId, id, StringLiteral(course.id))
            i.update(lwm.hasName, name, StringLiteral(course.name))
            i.update(rdfs.label, name, StringLiteral(course.name))
            system.eventStream.publish(Transaction(session.user, LocalDateTime.now(), ModifyAction(i.uri, s"Course modified by ${session.user}.")))
          }
          Future(Redirect(routes.CourseManagementController.index()))
        }
      ).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }
}
