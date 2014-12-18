package controllers

import models.{ ScheduleAssociations, LabWorks }
import play.api.mvc.{ Result, Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic._
import utils.semantic.Vocabulary.{ RDF, LWM }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Promise, Future }
import scala.util.control.NonFatal

object StudentInformationController extends Controller with Authentication with TransactionSupport {

  import play.api.Play.current
  override val system = Akka.system()

  def showInformation(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future.successful(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)))
    }
  }

  def postAlternateDate(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      ScheduleAssociations.Forms.alternateForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          Future.successful(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)))
        },
        dateChange ⇒ {
          val p = Promise[Result]()
          val schedule = Individual(Resource(dateChange.oldSchedule))
          val newSchedule = Resource(dateChange.newSchedule)
          schedule.props.get(LWM.hasAlternateScheduleAssociation) match {
            case None ⇒
              schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
              p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)))
            case Some(list) ⇒
              if (list.size > 0) {
                list.map { l ⇒
                  schedule.remove(LWM.hasAlternateScheduleAssociation, l)
                }
                schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
                p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)))
              } else {
                schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
                p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)))
              }
              modifyTransaction(session.user, schedule.uri, s"Alternate Schedule entry added to ${schedule.uri} by ${session.user}")
          }

          p.future
        }
      )
    }
  }

  def removeAlternateDate(scheduleId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action(parse.json) {
      implicit request ⇒
        import utils.Implicits._

        val student = (request.body \ "student").as[String]
        val oldSchedule = (request.body \ "oldSchedule").as[String]
        val alternate = (request.body \ "schedule").as[String]

        s"""
        |prefix lwm: <http://lwm.gm.fh-koeln.de/>
        |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |
        |delete data {
        |  <$student> lwm:hasSchedule <$oldSchedule> .
        |  <$oldSchedule> lwm:hasAlternateScheduleAssociation <$alternate> .
        |}
    """.stripMargin.execUpdate()

        Ok("Schedule removed")
    }
  }
}
