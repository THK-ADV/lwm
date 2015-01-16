
package controllers

import actors.SessionHandler
import actors.SessionHandler.{ Invalid, Valid }
import akka.util.Timeout
import controllers.UserManagement._
import models.UserForms
import play.api._
import play.api.mvc._
import play.libs.Akka
import utils.BreadCrumbKeeper
import utils.BreadCrumbKeeper.UrlReference
import utils.Global._
import utils.Security.Authentication
import utils.semantic.SPARQLTools
import utils.semantic.Vocabulary.{ lwm, foaf }

import scala.concurrent.Future

object Application extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import akka.pattern.ask

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

  override def onUnauthorized(request: RequestHeader): Result = Redirect(routes.Application.loginScreen())

  def index() = hasSession { session ⇒
    Action { implicit request ⇒
      session.role match {
        case Permissions.AdminRole ⇒
          Redirect(routes.AdministrationDashboardController.dashboard(DefaultBounds.min, DefaultBounds.max))
        case Permissions.DefaultRole ⇒
          Redirect(routes.StudentDashboardController.dashboard())
      }
    }
  }

  def loginScreen() = Action.async { implicit request ⇒
    val maybeToken = request.session.get("session")
    maybeToken match {
      case None ⇒
        Future.successful(Ok(views.html.login(UserForms.loginForm)))
      case Some(id) ⇒
        for {
          response ← sessionsHandler ? SessionHandler.SessionValidationRequest(id)
        } yield {
          response match {
            case Valid(session) ⇒
              Redirect(routes.Application.index())
            case _ ⇒
              Ok(views.html.login(UserForms.loginForm))
          }
        }
    }

  }

  def currentUser = hasSession {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val user = session.user

          def firstNameFuture = {
            val query = s"""
          |select ?s (${foaf.firstName} as ?p) ?o where {
          | ?s ${lwm.hasGmId} "$user" .
          | ?s ${foaf.firstName} ?o
          | }
        """.stripMargin

            sparqlExecutionContext.executeQuery(query).map { result ⇒
              SPARQLTools.statementsFromString(result).map(_.o)
            }
          }

          def lastNameFuture = {
            val query = s"""
          |select ?s (${foaf.lastName} as ?p) ?o where {
          | ?s ${lwm.hasGmId} "$user" .
          | ?s ${foaf.lastName} ?o
          | }
        """.stripMargin

            sparqlExecutionContext.executeQuery(query).map { result ⇒
              SPARQLTools.statementsFromString(result).map(_.o)
            }
          }
          for {
            firstName ← firstNameFuture
            lastName ← lastNameFuture
          } yield {
            Ok(s"${firstName.head} ${lastName.head} ($user)")
          }
      }
  }

  def refreshCrumbs = hasSession { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val label = (request.body \ "label").asOpt[String]
      val url = (request.body \ "url").asOpt[String]
      if (url.isDefined && label.isDefined && label.get.trim != BreadCrumbKeeper.noStorageRef) {
        session.breadcrumbKeeper.add(UrlReference(label.get, url.get))
      }
      Future.successful(Ok(session.breadcrumbKeeper.generate()))
    }
  }
}
