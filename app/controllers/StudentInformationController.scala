package controllers

import models.{ ScheduleAssociations, LabWorks }
import play.api.mvc.{ Result, Action, Controller }
import utils.Security.Authentication
import utils.semantic._
import utils.semantic.Vocabulary.{ RDF, LWM }
import utils.Global._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Promise, Future }
import scala.util.control.NonFatal

object StudentInformationController extends Controller with Authentication {
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
          }

          p.future
        }
      )
    }
  }

  def removeAlternateDate(scheduleId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val student = (request.body \ "student").asOpt[String]

        if (student.isDefined) {
          def statementRemoval(student: Resource, schedule: Resource) = {
            val query =
              s"""
            select distinct ?s (${LWM.hasAlternateScheduleAssociation} as ?p) ($schedule as ?o) where {
            $student ${LWM.hasScheduleAssociation} ?s .
            ?s ${LWM.hasAlternateScheduleAssociation} $schedule
            }
            """.stripMargin

            sparqlExecutionContext.executeQuery(query).map { result ⇒
              val statements = SPARQLTools.statementsFromString(result).map(r ⇒ (r.s, r.p, r.o))
              val update = buildQuery("delete", statements)
              //println(s"Remove: \n$update")
              sparqlExecutionContext.executeUpdate(update)
            }
          }

          def buildQuery(op: String, map: Seq[(Resource, Property, RDFNode)]): String = {
            val builder = new StringBuilder
            if (op == "delete") builder.append("DELETE DATA { ") else builder.append("INSERT DATA { ")
            map.foreach {
              e ⇒
                builder.append(s"${e._1} ${e._2} ${e._3.toQueryString} . ")
            }
            builder.deleteCharAt(builder.length - 2)
            builder.append("}")
            builder.toString()
          }

          (for {
            first ← statementRemoval(Resource(student.get), Resource(scheduleId))
            second ← first
          } yield {
            Redirect(routes.StudentInformationController.showInformation(student.get))
          }).recover {
            case NonFatal(t) ⇒ Redirect(routes.StudentInformationController.showInformation(student.get))
          }
        } else {
          Future.successful(Redirect(routes.StudentInformationController.showInformation(student.get)))
        }
    }
  }
}
