package controllers

import models._
import org.joda.time.LocalDate
import play.api.mvc.{ Action, Controller }
import utils.Global._
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ LWM, RDFS }
import utils.semantic.{ Individual, Resource, SPARQLTools }

import scala.concurrent.Future

object BlacklistManagementController extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      for {
        blacklists ← Blacklists.all()
        semesters ← Semesters.all()
      } yield {
        Ok(views.html.blacklist_management(blacklists, semesters, Blacklists.Forms.blacklistForm))
      }
    }
  }

  def blacklistEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      import utils.Global._
      val query =
        s"""
          |select ?s (${RDFS.label} as ?p) ?o where{
          | <$id> ${LWM.hasSemester} ?s .
          | ?s ${RDFS.label} ?o .
          |}
        """.stripMargin

      sparqlExecutionContext.executeQuery(query).flatMap { result ⇒
        BlacklistDates.getAll(Resource(id)).map { dates ⇒
          val title = SPARQLTools.statementsFromString(result).head.o.asLiteral().get.decodedString

          val semester = Individual(SPARQLTools.statementsFromString(result).head.s.asResource().get)
          val blacklist = Individual(Resource(id))

          Ok(views.html.blacklist_date_management(title, semester, dates, blacklist, Blacklists.Forms.blacklistDateForm))
        }
      }
    }
  }

  def blacklistPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Blacklists.Forms.blacklistForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            all ← Blacklists.all()
            semesters ← Semesters.all()
          } yield {
            BadRequest(views.html.blacklist_management(all, semesters, formWithErrors))
          }
        },
        blacklist ⇒ {
          Blacklists.create(Blacklist(Resource(blacklist.semesterResource))).map { i ⇒
            Redirect(routes.BlacklistManagementController.index())
          }
        }
      )
    }
  }

  def blacklistDatePost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Blacklists.Forms.blacklistDateForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            all ← Blacklists.all()
            semesters ← Semesters.all()
            dates ← BlacklistDates.getAll(Resource(formWithErrors.data("blacklistResource")))
          } yield {
            val blacklist = Individual(Resource(formWithErrors.data("blacklistResource")))
            val semester = Individual(Resource(formWithErrors.data("semesterResource")))

            val query =
              s"""
              |select ?s (${RDFS.label} as ?p) ?o where{
              | ${semester.uri} ${RDFS.label} ?o .
              |}
            """.stripMargin

            val result = sparqlExecutionContext.executeQueryBlocking(query)
            val title = SPARQLTools.statementsFromString(result).head.o.asLiteral().get.decodedString

            BadRequest(views.html.blacklist_date_management(title, semester, dates, blacklist, formWithErrors))

          }
        },
        blacklist ⇒ {
          BlacklistDates.create(BlacklistDate(Resource(blacklist.blacklistResource), new LocalDate(blacklist.date.getTime))).map { i ⇒
            Redirect(routes.BlacklistManagementController.blacklistEdit(blacklist.blacklistResource))
          }
        }
      )
    }
  }

  def blacklistDateDelete = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { request ⇒
      val listId = (request.body \ "listId").asOpt[String]
      val dateId = (request.body \ "dateId").asOpt[String]

      val maybeDeleted = for {
        list ← listId
        date ← dateId
      } yield BlacklistDates.delete(Resource(date))

      maybeDeleted match {
        case Some(deleted) ⇒ deleted.map { id ⇒
          Redirect(routes.BlacklistManagementController.blacklistEdit(id.value))
        }
        case None ⇒ Future.successful(Redirect(routes.BlacklistManagementController.index()))
      }
    }
  }
}
