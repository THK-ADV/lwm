package controllers

import models.{ ScheduleAssociations, LabWorks }
import play.api.libs.json.{ JsString, JsObject }
import play.api.mvc.{ Result, Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic._
import utils.semantic.Vocabulary.{ rdf, lwm }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Promise, Future }
import scala.util.control.NonFatal

object StudentInformationController extends Controller with Authentication with TransactionSupport {

  import play.api.Play.current
  override val system = Akka.system()

  def showInformation(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action { implicit request ⇒
      val start = System.nanoTime()
      val html = views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm)
      val duration = (System.nanoTime() - start) / 1000000
      println(s"Duration: $duration")
      Ok(html)
    }
  }

  def postAlternateDate(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val p = Promise[Result]()
      val os = (request.body \ "oldSchedule").as[String]
      val ns = (request.body \ "schedule").as[String]
      val student = (request.body \ "student").as[String]

      val schedule = Individual(Resource(os))
      val newSchedule = Resource(ns)
      schedule.props.get(lwm.hasAlternateScheduleAssociation) match {
        case None ⇒
          schedule.add(lwm.hasAlternateScheduleAssociation, newSchedule)
          modifyTransaction(session.user, schedule.uri, s"Alternate Schedule entry added to ${schedule.uri} of Student $student by ${session.user}")
          p.success(Ok(JsObject(Seq(
            "status" -> JsString("ok")
          ))))
        case Some(list) ⇒
          if (list.size > 0) {
            list.map { l ⇒
              schedule.remove(lwm.hasAlternateScheduleAssociation, l)
            }
            schedule.add(lwm.hasAlternateScheduleAssociation, newSchedule)
            p.success(Ok(JsObject(Seq(
              "status" -> JsString("ok")
            ))))
          } else {
            schedule.add(lwm.hasAlternateScheduleAssociation, newSchedule)
            p.success(Ok(JsObject(Seq(
              "status" -> JsString("ok")
            ))))
          }
          modifyTransaction(session.user, schedule.uri, s"Alternate Schedule entry added to ${schedule.uri} of Student $student by ${session.user}")
      }

      p.future
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
