package controllers

import models._
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.Resource
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by root on 9/13/14.
  */
object SemesterManagementController extends Controller with Authentication with TransactionSupport {

  override val system = Akka.system()

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          for (semesters ← Semesters.all()) yield {
            Ok(views.html.semesterManagement(semesters, Semesters.options, SemesterForm.semesterForm))
          }
      }
  }

  def semesterPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        SemesterForm.semesterForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            for (semester ← Semesters.all()) yield {
              BadRequest(views.html.semesterManagement(semester, Semesters.options, formWithErrors))
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
              createTransaction(session.user, s.uri, s"New Semester ${s.uri} created by ${session.user}")
              Redirect(routes.SemesterManagementController.index())
            }
          }
        )
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
          }
      }
  }
}
