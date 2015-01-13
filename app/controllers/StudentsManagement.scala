package controllers

import controllers.SemesterManagementController._
import models._
import play.api.libs.json.{ JsArray, JsString, Json }
import play.api.mvc.{ Result, Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.{ SPARQLTools, StringLiteral, Individual, Resource }
import utils.semantic.Vocabulary.{ lwm, rdfs, nco, foaf }
import utils.Global._
import scala.concurrent.{ Promise, Future }
import scala.util.control.NonFatal

object StudentsManagement extends Controller with Authentication with TransactionSupport {

  import scala.concurrent.ExecutionContext.Implicits.global

  import play.api.Play.current
  override val system = Akka.system()

  def index(page: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      for {
        students ← Students.all()
        degrees ← Degrees.all()
        s = students.map(i ⇒ Individual(i))
      } yield {
        val sorted = s.map(e ⇒ (e, e.props.getOrElse(lwm.hasEnrollment, List(Resource(""))).head.value)).sortBy(_._2)
        val paged = sorted.slice((page.toInt - 1) * 50, ((page.toInt - 1) * 50) + 50)
        val nrPages = (students.size / 50.0).round + 1
        Ok(views.html.studentManagement(paged, degrees, nrPages.toInt, UserForms.studentForm))
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
          val user = session.user
          val promise = Promise[Result]()

          Future {
            val exists = Students.exists(user)
            if (exists) {
              promise.success(Redirect(routes.StudentDashboardController.dashboard()))
            } else {
              if (student.gmId != user) {
                promise.success(Redirect(routes.FirstTimeSetupController.setupStudent()))
              } else {
                Students.create(student).map { s ⇒
                  createTransaction(session.user, s, s"New Student $s created by ${session.user}")
                  promise.success(Redirect(routes.StudentDashboardController.dashboard()))
                }
              }
            }
          }.recover {
            case NonFatal(e) ⇒ promise.success(Redirect(routes.Application.index()).withNewSession)
          }

          promise.future
        }
      )
    }
  }

  def studentPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            students ← Students.all()
            degrees ← Degrees.all()
            s = students.map(i ⇒ Individual(i))
          } yield {
            val sorted = s.map(e ⇒ (e, e.props.getOrElse(lwm.hasEnrollment, List(Resource(""))).head.value))
            val nrPages = (students.size / 50.0).round
            BadRequest(views.html.studentManagement(sorted, degrees, nrPages.toInt, formWithErrors))
          }
        },
        student ⇒ {
          Students.create(student).map { s ⇒
            createTransaction(session.user, s, s"New Student $s created by ${session.user}")
            Redirect(routes.StudentsManagement.index("1"))
          }
        }
      )
    }
  }

  def studentRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]

      val applicationQuery =
        s"""
          |select ?s (${lwm.hasApplicant} as ?p) (<$id> as ?o) where {
          | ?s ${lwm.hasApplicant} <$id>
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
        Students.delete(Resource(id)).map { s ⇒
          deleteTransaction(session.user, s, s"Student $s deleted by ${session.user}")
          Redirect(routes.StudentsManagement.index("1"))
        }
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

  def studentSearch(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Degrees.all().map(d ⇒ Ok(views.html.search_result_page(Individual(Resource(id)), d))).recover {
        case NonFatal(t) ⇒ Redirect(routes.LabworkManagementController.index())
      }

    }
  }

  def studentEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            students ← Students.all()
            degrees ← Degrees.all()
            s = students.map(i ⇒ Individual(i))
          } yield {
            val sorted = s.map(e ⇒ (e, e.props.getOrElse(lwm.hasEnrollment, List(Resource(""))).head.value))
            val nrPages = (students.size / 50.0).round
            BadRequest(views.html.studentManagement(sorted, degrees, nrPages.toInt, formWithErrors))
          }
        },
        student ⇒ {
          val s = Individual(Resource(id))
          for {
            id ← s.props.getOrElse(lwm.hasGmId, List(StringLiteral("")))
            firstName ← s.props.getOrElse(foaf.firstName, List(StringLiteral("")))
            lastName ← s.props.getOrElse(foaf.lastName, List(StringLiteral("")))
            regId ← s.props.getOrElse(lwm.hasRegistrationId, List(StringLiteral("")))
            email ← s.props.getOrElse(foaf.mbox, List(StringLiteral("")))
            phone ← s.props.getOrElse(nco.phoneNumber, List(StringLiteral("")))
            degree ← s.props.getOrElse(lwm.hasEnrollment, List(Resource("")))
            label ← s.props.getOrElse(rdfs.label, List(StringLiteral("")))
          } yield {
            s.update(lwm.hasGmId, id, StringLiteral(student.gmId))
            s.update(foaf.firstName, firstName, StringLiteral(student.firstname))
            s.update(foaf.lastName, lastName, StringLiteral(student.lastname))
            s.update(lwm.hasRegistrationId, regId, StringLiteral(student.registrationNumber))
            s.update(foaf.mbox, email, StringLiteral(student.email))
            s.update(nco.phoneNumber, phone, StringLiteral(student.phone))
            s.update(lwm.hasEnrollment, degree, Resource(student.degree))
            s.update(rdfs.label, label, StringLiteral(s"${student.firstname} ${student.lastname}"))
            modifyTransaction(session.user, s.uri, s"Student ${s.uri} modified by ${session.user}")
          }
          Future.successful(Redirect(routes.StudentsManagement.index("1")))
        }
      )
    }
  }

  def changeInformation(id: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          val s = Individual(Resource(id))
          UserForms.studentForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                degrees ← Degrees.all()
              } yield {
                BadRequest(views.html.dashboard_student_edit_details(s, degrees, formWithErrors))
              }
            },
            student ⇒ {
              val applicationQuery =
                s"""
                  |select ?s (${lwm.hasApplicant} as ?p) (<$id> as ?o) where {
                  | ?s ${lwm.hasApplicant} <$id>
                  |}
                  """.stripMargin

              val applicationFuture = sparqlExecutionContext.executeQuery(applicationQuery).map { result ⇒
                SPARQLTools.statementsFromString(result).map(_.s)
              }

              val degree = s.props.getOrElse(lwm.hasEnrollment, List(Resource(""))).head
              for {
                id ← s.props.getOrElse(lwm.hasGmId, List(StringLiteral("")))
                firstName ← s.props.getOrElse(foaf.firstName, List(StringLiteral("")))
                lastName ← s.props.getOrElse(foaf.lastName, List(StringLiteral("")))
                regId ← s.props.getOrElse(lwm.hasRegistrationId, List(StringLiteral("")))
                email ← s.props.getOrElse(foaf.mbox, List(StringLiteral("")))
                phone ← s.props.getOrElse(nco.phoneNumber, List(StringLiteral("")))
                label ← s.props.getOrElse(rdfs.label, List(StringLiteral("")))
              } yield {
                s.update(lwm.hasGmId, id, StringLiteral(student.gmId))
                s.update(foaf.firstName, firstName, StringLiteral(student.firstname))
                s.update(foaf.lastName, lastName, StringLiteral(student.lastname))
                s.update(lwm.hasRegistrationId, regId, StringLiteral(student.registrationNumber))
                s.update(foaf.mbox, email, StringLiteral(student.email))
                s.update(nco.phoneNumber, phone, StringLiteral(student.phone))
                s.update(lwm.hasEnrollment, degree, Resource(student.degree))
                s.update(rdfs.label, label, StringLiteral(s"${student.firstname} ${student.lastname}"))
                modifyTransaction(session.user, s.uri, s"Student ${s.uri} modified by ${session.user}")
              }
              if (degree.value != student.degree) {
                applicationFuture.flatMap { applications ⇒
                  Future.sequence(applications.map { application ⇒
                    LabworkApplications.delete(application)
                  })
                }.recover { case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.informationPage(id)) }
              }
            }
          )
          Future.successful(Redirect(routes.StudentDashboardController.dashboard()))
      }
  }
}
