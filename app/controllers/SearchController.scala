package controllers

import java.net.URLEncoder
import utils.Global._
import utils.Implicits._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ Resource, RDFNode, SPARQLTools }
import utils.semantic.Vocabulary.{ LWM, RDFS, FOAF }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object SearchController extends Controller with Authentication {

  def search(param: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val regex = s"${URLEncoder.encode(param, "UTF-8")}".toCharArray.toList.map(e ⇒ s"[$e]").mkString("")

      val futureFirstName = {
        val q = s"""
                Select ?s (${RDFS.label} as ?p) ?o where {
                ?s ${FOAF.firstName} ?name .
                ?s ${RDFS.label} ?o
                filter regex(?name, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureLastName = {
        val q = s"""
                Select ?s (${RDFS.label} as ?p) ?o where {
                ?s ${FOAF.lastName} ?name .
                ?s ${RDFS.label} ?o
                filter regex(?name, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureLabel = {
        val q = s"""
                Select ?s (${RDFS.label} as ?p) ?o where {
                ?s ${RDFS.label} ?o
                filter regex(?o, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureRegistrationId = {
        val q = s"""
                Select ?s (${RDFS.label} as ?p) ?o where {
                ?s ${LWM.hasRegistrationId} ?id .
                ?s ${RDFS.label} ?o
                filter regex(?id, "$regex", "i")
                } order by asc(?o)
              """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }

      val futureGmId = {
        val q = s"""
                 Select ?s (${LWM.hasGmId} as ?p) ?o {
                 ?s ${LWM.hasGmId} ?param .
                 ?s ${RDFS.label} ?o
                 filter regex(?param, '^$param','i')
                 } order by asc(?o)
               """.stripMargin
        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
        }
      }
      for {
        firstName ← futureFirstName
        lastName ← futureLastName
        registrationId ← futureRegistrationId
        gmId ← futureGmId
      } yield {
        val complete = firstName ++ lastName ++ registrationId ++ gmId
        Ok(views.html.complex_search(complete))
      }
    }
  }
}
