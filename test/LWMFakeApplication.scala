import actors.{EmailHandler, SessionHandler}
import actors.SessionHandler.Session
import akka.actor.{Actor, Props}
import controllers.Permissions
import org.joda.time.DateTime
import org.scalatestplus.play.OneAppPerSuite
import play.api.libs.concurrent.Akka
import play.api.test.FakeApplication
import play.api.{Application, GlobalSettings}

trait LWMFakeApplication {self: OneAppPerSuite =>
  lazy val fakeGlobals = new GlobalSettings {

    override def onStart(app: Application) {
      import play.api.Play.current
      Akka.system.actorOf(Props(new Actor {
        val fakeSession = Session("fakeId", DateTime.now, "fakeUser", List(), Permissions.AdminRole)

        override def receive: Receive = {
          case SessionHandler.AuthenticationRequest(user, password) =>
            sender() ! fakeSession
          case SessionHandler.LogoutRequest(user) =>
          case SessionHandler.NameRequest(user) =>

            sender() ! ("Fake", "User")
          case SessionHandler.SessionRequest(id) =>
            sender() ! fakeSession
          case SessionHandler.SessionValidationRequest(id) =>
            sender() ! SessionHandler.Valid(fakeSession)
        }
      }), "sessions")

      Akka.system.actorOf(Props(new Actor {
        val fakeSession = Session("fakeId", DateTime.now, "fakeUser", List(), Permissions.AdminRole)

        override def receive: Receive = {
          case EmailHandler.MessageRequest(count) =>
            sender() ! EmailHandler.EmailResponse(Nil)
        }
      }), "emails")
    }

  }

  override implicit lazy val app: FakeApplication = FakeApplication(withGlobal = Some(fakeGlobals))
}
