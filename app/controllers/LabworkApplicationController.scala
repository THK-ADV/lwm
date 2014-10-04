package controllers

import models._
import play.api.mvc.{ Action, Controller }
import utils.ListGrouping
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ SPARQLTools, Individual, Resource, StringLiteral }

import scala.concurrent.Future
import scala.util.control.NonFatal

object LabworkApplicationController extends Controller with Authentication {

  import utils.Global._

  import scala.concurrent.ExecutionContext.Implicits.global

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒

      val t = LabworkApplicationLists.all().flatMap { lists ⇒
        Future.sequence(getApplicationListInfo(lists))
      }

      for (info ← t) yield Ok(views.html.labwork_application_management(info))
    }
  }

  private def getApplicationListInfo(lists: List[Individual]) = {
    def semester(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${LWM.hasSemester} as ?p) ?o where {
             | $list ${LWM.hasLabWork} ?labwork .
             | ?labwork ${LWM.hasSemester} ?semester .
             | ?semester ${RDFS.label} ?o .
             |}
           """.stripMargin

      sparqlExecutionContext.executeQuery(query).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
      }
    }

    def degree(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${LWM.hasDegree} as ?p) ?o where {
             | $list ${LWM.hasLabWork} ?labwork .
             | ?labwork ${LWM.hasCourse} ?course .
             | ?course ${LWM.hasDegree} ?degree .
             | ?degree ${RDFS.label} ?o .
             |}
           """.stripMargin

      sparqlExecutionContext.executeQuery(query).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
      }
    }

    def applications(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${LWM.hasApplication} as ?p) ?o where {
             | $list ${LWM.hasApplication} ?o .
             |}
           """.stripMargin

      sparqlExecutionContext.executeQuery(query).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
      }
    }

    for (list ← lists) yield {
      for {
        ss ← semester(list.uri)
        dd ← degree(list.uri)
        aa ← applications(list.uri)
      } yield (list, ss.head.toString, dd.head.toString, aa.size)
    }

  }

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

                // Get resources for all known labwork partners
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

  def applicationListEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      import utils.Global._

      val applicationlist = Individual(Resource(id))

      val applicationsFuture = LabworkApplicationLists.getAllApplications(applicationlist.uri)

      applicationsFuture.map { applications ⇒
        Ok(views.html.labwork_application_list_details(applicationlist, applications))
      }.recover {
        case NonFatal(t) ⇒
          Redirect(routes.LabworkApplicationController.index())
      }
    }
  }

  def groupList(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      import utils.Global._

      val applicationlist = Individual(Resource(id))
      println("Init Grouping")
      val applicationsFuture = LabworkApplicationLists.getAllApplications(applicationlist.uri)
      applicationsFuture.map { applications ⇒
        println(applications)
        applicationlist.props.get(LWM.hasLabWork).map { labwork ⇒
          ListGrouping.group(labwork.head.asResource().get, applications.map(_.uri), 1, 1)
        }
      }

      Future.successful(Redirect(routes.LabworkApplicationController.index()))
    }
  }

}
