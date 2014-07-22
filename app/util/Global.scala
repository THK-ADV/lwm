package util

import controllers.SessionHandler
import play.api._
import play.api.libs.concurrent.Akka

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val application = app
    println(Akka.system.actorOf(SessionHandler.props(app.configuration), "sessions"))
    Logger.info("Application has started")
  }

  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
  }

}