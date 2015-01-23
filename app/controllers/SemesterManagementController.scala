package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.{ Individual, Resource }
import scala.concurrent.ExecutionContext.Implicits.global
import utils.Global._

import scala.util.control.NonFatal

/**
  * Created by root on 9/13/14.
  */
object SemesterManagementController extends Controller with Authentication with TransactionSupport {

  override val system = Akka.system()

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          (for {
            semesterResources ← Semesters.all()
            semesters = semesterResources.map(s ⇒ Individual(s))
          } yield {
            Ok(views.html.semesterManagement(semesters, Semesters.options, SemesterForm.semesterForm))
          }).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def semesterPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        SemesterForm.semesterForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            for {
              semesterResources ← Semesters.all()
              semesters = semesterResources.map(s ⇒ Individual(s))
            } yield {
              BadRequest(views.html.semesterManagement(semesters, Semesters.options, formWithErrors))
            }
          },
          s ⇒ {
            val semester: Semester = s.semester.toLowerCase match {
              case "sommersemester" ⇒
                SummerSemester(s.year)
              case _ ⇒
                WinterSemester(s.year)
            }
            Semesters.create(semester).map { s ⇒
              createTransaction(session.user, s, s"New Semester $s created by ${session.user}")
              Redirect(routes.SemesterManagementController.index())
            }
          }
        ).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def semesterRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          Semesters.delete(Resource(id)).map { s ⇒
            deleteTransaction(session.user, s, s"New Semester $s created by ${session.user}")
            Redirect(routes.SemesterManagementController.index())
          }.recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }
}
