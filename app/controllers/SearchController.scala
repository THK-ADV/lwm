package controllers

import java.net.URLEncoder
import controllers.AdministrationDashboardController._
import utils.Global._
import utils.Implicits._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ Resource, RDFNode, SPARQLTools }
import utils.semantic.Vocabulary.{ lwm, rdfs, foaf }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

object SearchController extends Controller with Authentication {

  def search(param: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val regex = s"${URLEncoder.encode(param, "UTF-8")}".toCharArray.toList.map(e ⇒ s"[$e]").mkString("")

      val futureFirstName = {
        val q = s"""
                Select ?s (${rdfs.label} as ?p) ?o where {
                ?s ${foaf.firstName} ?name .
                ?s ${rdfs.label} ?o
                filter regex(?name, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureLastName = {
        val q = s"""
                Select ?s (${rdfs.label} as ?p) ?o where {
                ?s ${foaf.lastName} ?name .
                ?s ${rdfs.label} ?o
                filter regex(?name, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureLabel = {
        val q = s"""
                Select ?s (${rdfs.label} as ?p) ?o where {
                ?s ${rdfs.label} ?o
                filter regex(?o, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureRegistrationId = {
        val q = s"""
                Select ?s (${rdfs.label} as ?p) ?o where {
                ?s ${lwm.hasRegistrationId} ?id .
                ?s ${rdfs.label} ?o
                filter regex(?id, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureGmId = {
        val q = s"""
                 Select ?s (${lwm.hasGmId} as ?p) ?o {
                 ?s ${lwm.hasGmId} ?param .
                 ?s ${rdfs.label} ?o
                 filter regex(?param, '^$param','i')
                 } order by asc(?o)
               """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }
      (for {
        firstName ← futureFirstName
        lastName ← futureLastName
        registrationId ← futureRegistrationId
        gmId ← futureGmId
      } yield {
        val complete = firstName ++ lastName ++ registrationId ++ gmId
        Ok(views.html.complex_search(complete))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }
}
