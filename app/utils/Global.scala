package utils

import actors.{ OntologyDumperActor, EmailHandler, SessionHandler }
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

  implicit val timeout = Timeout(30.seconds)
  implicit val sparqlExecutionContext = SPARQLExecution(queryHost, updateHost)
  val lwmNamespace = Namespace("http://lwm.gm.fh-koeln.de/")

  override def onStart(app: Application) {
    Akka.system.actorOf(SessionHandler.props(app.configuration), "sessions")
    Akka.system.actorOf(UserInfoManagement.props(app.configuration), "user-info")
    Akka.system.actorOf(EmailHandler.props(app.configuration), "emails")
    Akka.system.actorOf(OntologyDumperActor.props(dataInf), "dumper")
    Logger.debug("Application has started")
  }

  override def onStop(app: Application) {
    Logger.debug("Application shutdown...")
  }

  override def onRouteRequest(req: RequestHeader): Option[Handler] = {
    println(req)
    println(req.host)
    println(req.uri)
    println(req.headers)
    println(Play.isProd)
    (req.method, req.headers.get("X-Forwarded-Proto")) match {
      case (_, _) ⇒
        Some(Action { request ⇒
          TemporaryRedirect("https://" + req.host + req.uri)
        })
    }
  }
}