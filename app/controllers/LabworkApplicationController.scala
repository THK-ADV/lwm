package controllers

import models.{ LabworkApplication, LabworkApplications, Students }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ Individual, Resource, StringLiteral }

import scala.concurrent.Future
import scala.util.control.NonFatal

object LabworkApplicationController extends Controller with Authentication {

  import utils.Global._

  import scala.concurrent.ExecutionContext.Implicits.global

  def labworkApplicationPost() = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        LabworkApplications.Forms.labworkApplicationForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            Future.successful(Redirect(routes.StudentDashboardController.dashboard()))
          },
          s ⇒ {
            Students.get(s.applicant).flatMap { applicantResource ⇒
              val labworkIndividual = Individual(Resource(s.labwork))

              val applicationsAllowed = labworkIndividual.props.getOrElse(LWM.allowsApplications, List(StringLiteral("false"))).head.value.toBoolean
              if (applicationsAllowed) {
                val partnersFuture = s.partners.map(Students.get)
                val partnerList = partnersFuture.foldLeft(Future.successful(List.empty[Resource])) { (partners, fut) ⇒
                  partners.flatMap { list ⇒
                    fut.map { res ⇒
                      res :: list
                    }.recover {
                      case NonFatal(t) ⇒ list
                    }
                  }
                }

                partnerList.flatMap { list ⇒
                  val labworkApplication = LabworkApplication(applicantResource, Resource(s.labwork), list)
                  LabworkApplications.create(labworkApplication).map { l ⇒
                    Redirect(routes.StudentDashboardController.dashboard())
                  }
                }

              } else {
                Future.successful(Redirect(routes.StudentDashboardController.dashboard()))
              }
            }.recover {
              case NonFatal(t) ⇒
                Redirect(routes.StudentDashboardController.dashboard())
            }

          }
        )
      }
  }
}
