package controllers

import models._
import play.api.libs.json.{ JsArray, JsString, Json }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ SPARQLTools, StringLiteral, Individual, Resource }
import utils.semantic.Vocabulary.{ LWM, RDFS, NCO, FOAF }
import utils.Global._
import scala.concurrent.Future

object StudentsManagement extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      for {
        students ← Students.all()
        degrees ← Degrees.all()
      } yield {
        Ok(views.html.studentManagement(students.toList, degrees, UserForms.studentForm))
      }
    }
  }

  def studentFirstTimeSelf = hasSession { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            degrees ← Degrees.all()
          } yield BadRequest(views.html.firstTimeInputStudents(degrees, formWithErrors))
        },
        student ⇒ {
          Students.create(student).map(_ ⇒ Redirect(routes.StudentDashboardController.dashboard()))

        }
      )
    }
  }

  def studentPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            all ← Students.all()
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.studentManagement(all.toList, degrees, formWithErrors))
          }
        },
        student ⇒ {
          Students.create(student).map(_ ⇒ Redirect(routes.StudentsManagement.index()))
        }
      )
    }
  }

  def studentRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]

      val applicationQuery =
        s"""
          |select ?s (${LWM.hasApplicant} as ?p) (<$id> as ?o) where {
          | ?s ${LWM.hasApplicant} <$id>
          |}
        """.stripMargin

      val applicationFuture = sparqlExecutionContext.executeQuery(applicationQuery).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.s)
      }

      applicationFuture.flatMap { applications ⇒
        Future.sequence(applications.map { application ⇒
          LabworkApplications.delete(application)
        })
      }.flatMap { hui ⇒
        Students.delete(Resource(id)).map(_ ⇒ Redirect(routes.StudentsManagement.index()))
      }
    }
  }

  def studentSuggestions = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val query = request.queryString.getOrElse("term", List("")).head
      Students.search(query, -1).map { suggestions ⇒

        Ok(JsArray(suggestions.map(s ⇒ Json.obj(
          "label" -> JsString(s"(${s._1}) ${s._2}"),
          "value" -> JsString(s._3)
        ))).toString())
      }

    }
  }

  def studentEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            all ← Students.all()
            degrees ← Degrees.all()
          } yield {
            BadRequest(views.html.studentManagement(all.toList, degrees, formWithErrors))
          }
        },
        student ⇒ {
          val s = Individual(Resource(id))
          for {
            id ← s.props.getOrElse(LWM.hasGmId, List(StringLiteral("")))
            firstName ← s.props.getOrElse(FOAF.firstName, List(StringLiteral("")))
            lastName ← s.props.getOrElse(FOAF.lastName, List(StringLiteral("")))
            regId ← s.props.getOrElse(LWM.hasRegistrationId, List(StringLiteral("")))
            email ← s.props.getOrElse(FOAF.mbox, List(StringLiteral("")))
            phone ← s.props.getOrElse(NCO.phoneNumber, List(StringLiteral("")))
            degree ← s.props.getOrElse(LWM.hasEnrollment, List(Resource("")))
          } yield {
            s.update(LWM.hasGmId, id, StringLiteral(student.gmId))
            s.update(FOAF.firstName, firstName, StringLiteral(student.firstname))
            s.update(FOAF.lastName, lastName, StringLiteral(student.lastname))
            s.update(LWM.hasRegistrationId, regId, StringLiteral(student.registrationNumber))
            s.update(FOAF.mbox, email, StringLiteral(student.email))
            s.update(NCO.phoneNumber, phone, StringLiteral(student.phone))
            s.update(LWM.hasEnrollment, degree, Resource(student.degree))
          }
          Future.successful(Redirect(routes.StudentsManagement.index()))
        }
      )
    }
  }
}
