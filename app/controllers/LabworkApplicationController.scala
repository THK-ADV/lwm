package controllers

import controllers.AdministrationDashboardController._
import models._
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ foaf, lwm, rdf, rdfs }
import utils.semantic.{ Individual, Resource, SPARQLTools, StringLiteral }
import utils.{ ListGrouping, TransactionSupport }

import scala.concurrent.Future
import scala.util.control.NonFatal

object LabworkApplicationController extends Controller with Authentication with TransactionSupport {

  import utils.Global._

  import scala.concurrent.ExecutionContext.Implicits.global

  override val system = Akka.system()

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      val t = LabworkApplicationLists.all().flatMap { lists ⇒
        Future.sequence(getApplicationListInfo(lists))
      }
      (for (info ← t) yield Ok(views.html.labwork_application_management(info))).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  private def getApplicationListInfo(lists: List[Individual]) = {
    def semester(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${lwm.hasSemester} as ?p) ?o where {
             | $list ${lwm.hasLabWork} ?labwork .
             | ?labwork ${lwm.hasSemester} ?semester .
             | ?semester ${rdfs.label} ?o .
             |}
           """.stripMargin

      sparqlExecutionContext.executeQuery(query).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
      }
    }

    def degree(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${lwm.hasDegree} as ?p) ?o where {
             | $list ${lwm.hasLabWork} ?labwork .
             | ?labwork ${lwm.hasCourse} ?course .
             | ?course ${lwm.hasDegree} ?degree .
             | ?degree ${rdfs.label} ?o .
             |}
           """.stripMargin

      sparqlExecutionContext.executeQuery(query).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o.asLiteral()).flatten
      }
    }

    def applications(list: Resource) = {
      val query =
        s"""
             |select ($list as ?s) (${lwm.hasApplication} as ?p) ?o where {
             | $list ${lwm.hasApplication} ?o .
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
            Future(Redirect(routes.StudentDashboardController.dashboard()))
          },
          s ⇒ {
            Students.get(s.applicant).flatMap { applicantResource ⇒
              val labworkIndividual = Individual(Resource(s.labwork))

              val applicationsAllowed = labworkIndividual.props.getOrElse(lwm.allowsApplications, List(StringLiteral("false"))).head.value.toBoolean
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
                    createTransaction(session.user, l.uri, s"Labwork Application ${l.uri} created by ${session.user}")
                    Redirect(routes.StudentDashboardController.dashboard())
                  }.recover {
                    case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard())
                  }
                }.recover {
                  case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard())
                }

              } else {
                Future(Redirect(routes.StudentDashboardController.dashboard()))
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
    Action.async { implicit request ⇒
      import utils.Global._

      val applicationlist = Individual(Resource(id))

      val applicationsFuture = LabworkApplicationLists.getAllApplications(applicationlist.uri)

      def openLabs = {
        val query =
          s"""
             |select ?s (${lwm.allowsApplications} as ?p) ?o where {
             | ?s ${lwm.allowsApplications} "true" .
             | ?s ${lwm.hasCourse} ?course .
             | ?course ${lwm.hasDegree} ?degree .
             | ?degree ${rdfs.label} ?o
             |}
           """.stripMargin

        sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(t ⇒ (t.s, t.o))
        }
      }

      applicationsFuture.flatMap { applications ⇒
        val mapped = applications.map(e ⇒ (e, e.props.getOrElse(lwm.hasApplicant, List(Resource(""))).map(s ⇒ Individual(s.asResource().get)).head))
        val sorted = mapped.sortBy(_._2.props.getOrElse(foaf.lastName, List(StringLiteral(""))).head.value)
        openLabs.map { a ⇒
          Ok(views.html.labwork_application_list_details(a.toList.map(r ⇒ (Individual(r._1), r._2.value)), applicationlist, sorted))
        }
      }.recover {
        case NonFatal(t) ⇒
          Redirect(routes.LabworkApplicationController.index())
      }
    }
  }

  def groupList(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      import utils.Global._
      val min = (request.body \ "min").asOpt[String]
      val max = (request.body \ "max").asOpt[String]
      if (min.isDefined && max.isDefined) {
        val applicationlist = Individual(Resource(id))
        val applicationsFuture = LabworkApplicationLists.getAllApplications(applicationlist.uri)
        applicationsFuture.map { applications ⇒
          applicationlist.props.get(lwm.hasLabWork).map { labwork ⇒
            ListGrouping.group(labwork.head.asResource().get, applications.map(_.uri), min.get.toInt, max.get.toInt) // TODO -> has to be in application.conf
          }
        }.recover {
          case NonFatal(t) ⇒ Redirect(routes.LabworkApplicationController.index())
        }
      }
      Future(Redirect(routes.LabworkApplicationController.index())).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def adminApplicationRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val applicationd = (request.body \ "app").asOpt[String]
          val listId = (request.body \ "list").asOpt[String]
          if (applicationd.isDefined && listId.isDefined) {
            LabworkApplications.delete(Resource(applicationd.get)).map(_ ⇒ Redirect(routes.LabworkApplicationController.applicationListEdit(listId.get))).recover { case NonFatal(t) ⇒ routes.LabworkApplicationController.index() }
          }
          Future(Redirect(routes.LabworkApplicationController.index())).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def studentApplicationRemoval = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val lab = (request.body \ "lab").asOpt[String]
          val s = (request.body \ "s").asOpt[String]

          if (lab.isDefined && s.isDefined) {

            def applications(labwork: Resource) = {
              val query =
                s"""
             |select ?s (${lwm.hasLabWork} as ?p) ($labwork as ?o) where {
             | ?s ${lwm.hasLabWork} $labwork .
             | ?s ${lwm.hasApplicant} <${s.get}>
             |}
           """.stripMargin

              sparqlExecutionContext.executeQuery(query).map { result ⇒
                SPARQLTools.statementsFromString(result).map(_.s)
              }
            }

            (for (application ← applications(Resource(lab.get))) yield {
              if (application.nonEmpty) {
                LabworkApplications.delete(application.head).map { deleted ⇒
                  deleteTransaction(session.user, deleted, s"Labwork Application removed by ${session.user}")
                  Redirect(routes.StudentDashboardController.dashboard())
                }
              }
            }).recover {
              case NonFatal(t) ⇒ Redirect(routes.StudentDashboardController.dashboard())
            }
          }
          Future(Redirect(routes.StudentDashboardController.dashboard())).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }

      }
  }

  def changeLists = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        LabworkApplications.Forms.labworkApplicationChangeForm.bindFromRequest.fold(
          formWithErrors ⇒ {
            Future.successful(Redirect(routes.LabworkApplicationController.index()))
          },
          e ⇒ {
            val labwork = Resource(e.labwork)
            val application = Individual(Resource(e.application))
            val oldLabwork = application.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head.asResource().get

            def oldApplicationList(labwork: Resource) = {
              val query =
                s"""
             |select ?s (${lwm.hasLabWork} as ?p) ($labwork as ?o) where {
             | $labwork ${lwm.hasApplicationList} ?s .
             |}
           """.stripMargin

              sparqlExecutionContext.executeQuery(query).map { result ⇒
                SPARQLTools.statementsFromString(result).map(_.s)
              }
            }

            def newApplicationList(labwork: Resource) = {
              val query =
                s"""
             |select ?s (${lwm.hasLabWork} as ?p) ($labwork as ?o) where {
             | ?s ${rdf.typ} ${lwm.LabworkApplicationList} .
             | ?s ${lwm.hasLabWork} $labwork
             |}
           """.stripMargin

              sparqlExecutionContext.executeQuery(query).map { result ⇒
                SPARQLTools.statementsFromString(result).map(_.s)
              }
            }

            (for {
              oldList ← oldApplicationList(oldLabwork)
              newList ← newApplicationList(labwork)
            } yield {
              for {
                d1 ← oldList
                d2 ← newList
              } yield {
                Individual(d1).remove(lwm.hasApplication, application.uri)
                Individual(d2).add(lwm.hasApplication, application.uri)
                application.update(lwm.hasLabWork, oldLabwork, labwork)
              }
              Redirect(routes.LabworkApplicationController.index())
            }).recover {
              case NonFatal(t) ⇒ Redirect(routes.LabworkApplicationController.index())
            }
          })
        Future(Redirect(routes.LabworkApplicationController.index())).recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
    }
  }

  def addApplication(listId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val student = (request.body \ "student").asOpt[String]
      if (student.isDefined) {
        val listI = Individual(Resource(listId))
        val labwork = listI.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head
        LabworkApplications.create(LabworkApplication(Resource(student.get), labwork.asResource().get, Nil)).map { e ⇒
          createTransaction(session.user, e.uri, s"Labwork Application created by ${session.user}")
          listI.add(lwm.hasApplication, e.uri)
          Redirect(routes.LabworkApplicationController.applicationListEdit(listId))
        }.recover { case NonFatal(t) ⇒ routes.LabworkApplicationController.applicationListEdit(listId) }
      }
      Future(Redirect(routes.LabworkApplicationController.index())).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

}
