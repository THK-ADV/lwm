package utils

import actors.{ TransactionsLoggerActor, OntologyDumperActor, EmailHandler, SessionHandler }
import akka.util.Timeout
import controllers.UserInfoManagement
import models.{ Students, Student }
import play.api.mvc._
import play.api.mvc.Results._
import play.api.{ Play, Logger, Application, GlobalSettings }
import play.api.libs.concurrent.Akka
import play.api.Play.current
import utils.semantic.{ Vocabulary, SPARQLExecution, Namespace, NamedGraph }

import scala.concurrent.ExecutionContext

object Global extends GlobalSettings {

  import scala.concurrent.duration._

  lazy val port = 3030
  lazy val serviceName = "lwm"
  lazy val updateHost = s"http://localhost:$port/$serviceName/update"
  lazy val queryHost = s"http://localhost:$port/$serviceName/query"
  lazy val dataInf = s"http://localhost:$port/$serviceName/data"
  implicit lazy val query = QueryHost(queryHost)
  implicit lazy val update = UpdateHost(updateHost)

  implicit val timeout = Timeout(5.seconds)
  implicit val sparqlExecutionContext = SPARQLExecution(queryHost, updateHost)
  val lwmNamespace = Namespace("http://lwm.gm.fh-koeln.de/")

  override def onStart(app: Application) {
    Akka.system.actorOf(SessionHandler.props(app.configuration), "sessions")
    Akka.system.actorOf(UserInfoManagement.props(app.configuration), "user-info")
    // Akka.system.actorOf(EmailHandler.props(app.configuration), "emails")
    Akka.system.actorOf(OntologyDumperActor.props(dataInf, app.configuration.getInt("lwm.backup.interval").getOrElse(30).minutes), "dumper")
    Akka.system.actorOf(TransactionsLoggerActor.props(), "logger")
    Logger.debug("Application has started")
  }

  override def onStop(app: Application) {
    Logger.debug("Application shutdown...")
  }

  override def onRouteRequest(req: RequestHeader): Option[Handler] = {
    if (Play.isDev) {
      super.onRouteRequest(req)
    } else {
      if (req.secure) {
        super.onRouteRequest(req)
      } else {
        val secureHost = "https://lwivs18.gm.fh-koeln.de:9443"
        val secureUrl = s"$secureHost${req.uri}"
        Some(Action(_ ⇒ Redirect(secureUrl)))
      }
    }
  }
}
