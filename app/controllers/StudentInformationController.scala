package controllers

import models.{ ScheduleAssociations, LabWorks }
import play.api.mvc.{ Result, Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ Individual, Resource }
import utils.semantic.Vocabulary.LWM
import utils.Global._

import scala.concurrent.{ Promise, Future }

object StudentInformationController extends Controller with Authentication {
  def showInformation(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future.successful(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm, session)))
    }
  }

  def postAlternateDate(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      ScheduleAssociations.Forms.alternateForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          Future.successful(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm, session)))
        },
        dateChange ⇒ {
          val p = Promise[Result]()
          val schedule = Individual(Resource(dateChange.oldSchedule))
          val newSchedule = Resource(dateChange.newSchedule)
          schedule.props.get(LWM.hasAlternateScheduleAssociation) match {
            case None ⇒
              schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
              p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm, session)))
            case Some(list) ⇒
              if (list.size > 0) {
                list.map { l ⇒
                  schedule.remove(LWM.hasAlternateScheduleAssociation, l)
                }
                schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
                p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm, session)))
              } else {
                schedule.add(LWM.hasAlternateScheduleAssociation, newSchedule)
                p.success(Ok(views.html.student_information_overview(Resource(id), ScheduleAssociations.Forms.alternateForm, session)))
              }
          }

          p.future
        }
      )
    }
  }
}
