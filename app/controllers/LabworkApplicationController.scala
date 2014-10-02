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
            println(formWithErrors)
            Future.successful(BadRequest(""))
          },
          s ⇒ {
            Students.get(s.applicant).flatMap { applicantResource ⇒
              println(applicantResource)
              val applicantIndividual = Individual(applicantResource)
              val labworkIndividual = Individual(Resource(s.labwork))

              val applicationsAllowed = labworkIndividual.props.getOrElse(LWM.allowsApplications, List(StringLiteral("false"))).head.value.toBoolean
              println(applicationsAllowed)
              if (applicationsAllowed) {
                val partnersFuture = s.partners.map(Students.get)
                val partnerList = partnersFuture.foldLeft(Future.successful(List.empty[Resource])) { (partners, fut) ⇒
                  fut.flatMap { res ⇒
                    partners.map { list ⇒
                      res :: list
                    }
                  }.recover {
                    case NonFatal(t) ⇒ Nil
                  }
                }

                partnerList.flatMap { list ⇒
                  println(list)
                  val labworkApplication = LabworkApplication(applicantResource, Resource(s.labwork), list)
                  LabworkApplications.create(labworkApplication).map { l ⇒
                    println(s"Created $l")
                    Redirect(routes.StudentDashboardController.dashboard())
                  }
                }

              } else {
                Future.successful(BadRequest(""))
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
