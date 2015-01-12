package controllers

import java.net.URLEncoder

import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic._
import utils.semantic.Vocabulary.{ RDFS, RDF }
import utils.Global._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

/**
  * Created by root on 11/18/14.
  */
object SuperUser extends Controller with Authentication {

  def index = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future.successful(Ok(views.html.helpers.sudo.mainPage()))
    }
  }

  def resourceOverview(typ: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val q =
        s"""
          Select ?s (${RDFS.label} as ?p) ?o where {
          ?s ${RDF.typ} <$typ> .
          ?s ${RDFS.label} ?o
          }
        """.stripMargin

      val resourcesFuture = sparqlExecutionContext.executeQuery(q).map { result ⇒
        SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.o))
      }
      resourcesFuture.map(e ⇒ Ok(views.html.helpers.sudo.overviewPage(Resource(typ), e.toList))).recover { case NonFatal(t) ⇒ Redirect(routes.SuperUser.index()) }
    }
  }

  def resourceDetails(typ: String, resource: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      val q =
        s"""
          Select (<$resource> as ?s) ?p ?o where {
          <$resource> ?p ?o
          filter(?p != ${RDF.typ})
          }
        """.stripMargin

      val resourcesFuture = sparqlExecutionContext.executeQuery(q).map { result ⇒
        SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p, e.o))
      }
      resourcesFuture.map(e ⇒ Ok(views.html.helpers.sudo.detailsPage(Resource(resource), e.toList))).recover { case NonFatal(t) ⇒ Redirect(routes.SuperUser.index()) }
    }
  }

  def statementRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val r = (request.body \ "resource").asOpt[String]
      val p = (request.body \ "property").asOpt[String]
      val n = (request.body \ "node").asOpt[String]

      if (r.isDefined && p.isDefined && n.isDefined) {
        val resource = Resource(r.get)
        val property = Property(p.get)
        val rdfnode = {
          if (n.get.contains("http")) Resource(n.get)
          else s"'${URLEncoder.encode(n.get, "UTF-8")}'"
        }
        val u =
          s"""
              |Delete data {
              |$resource $property $rdfnode
              |}
            """.stripMargin

        sparqlExecutionContext.executeUpdate(u).map(_ ⇒ Redirect(routes.SuperUser.resourceOverview(r.get))).recover { case NonFatal(t) ⇒ Redirect(routes.SuperUser.index()) }

      } else {
        Future.successful(Ok(views.html.helpers.sudo.mainPage()))
      }
    }
  }

  def statementUpdate() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒

      val prevRes = (request.body \ "prevRes").asOpt[String]
      val prevProp = (request.body \ "prevProp").asOpt[String]
      val prevNode = (request.body \ "prevNode").asOpt[String]
      val nextRes = (request.body \ "nextRes").asOpt[String]
      val nextProp = (request.body \ "nextProp").asOpt[String]
      val nextNode = (request.body \ "nextNode").asOpt[String]

      val url = (request.body \ "url").asOpt[String]
      if (prevRes.isDefined && prevProp.isDefined && prevNode.isDefined && nextRes.isDefined && nextProp.isDefined && nextNode.isDefined) {
        val pn = if (prevNode.get.contains("http")) prevNode.get; else s"'${URLEncoder.encode(prevNode.get, "UTF-8")}'"
        val nn = if (nextNode.get.contains("http")) nextNode.get; else s"'${URLEncoder.encode(nextNode.get, "UTF-8")}'"
        val u =
          s"""
            Delete data {
            ${prevRes.get} ${prevProp.get} $pn
            };
            Insert data {
            ${nextRes.get} ${nextProp.get} $nn
            }
          """.stripMargin
        sparqlExecutionContext.executeUpdate(u).map(_ ⇒ Redirect(url.get)).recover { case NonFatal(t) ⇒ Redirect(url.get) }
        Future.successful(Redirect(url.get))
      } else {
        Future.successful(Redirect(url.get))
      }
    }
  }
}
