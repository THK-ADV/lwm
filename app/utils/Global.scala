package utils

import actors.{ EmailHandler, SessionHandler }
import akka.util.Timeout
import controllers.UserInfoManagement
import models.{ Students, Student }
import org.apache.jena.fuseki.FusekiCmd
import play.api._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import utils.semantic.{ Vocabulary, SPARQLExecution, Namespace, NamedGraph }

import scala.concurrent.ExecutionContext

object Global extends GlobalSettings {

  import scala.concurrent.duration._

  private val updateHost = "http://localhost:3030/lwm/update"
  private val queryHost = "http://localhost:3030/lwm/query"

  implicit val timeout = Timeout(30.seconds)
  implicit val sparqlExecutionContext = SPARQLExecution(queryHost, updateHost)
  val lwmNamespace = Namespace("http://lwm.gm.fh-koeln.de/")

  override def onStart(app: Application) {
    Akka.system.actorOf(SessionHandler.props(app.configuration), "sessions")
    Akka.system.actorOf(UserInfoManagement.props(app.configuration), "user-info")
    //Akka.system.actorOf(EmailHandler.props(app.configuration), "emails")
    Logger.debug("Application has started")
  }

  override def onStop(app: Application) {
    Logger.debug("Application shutdown...")
  }

}