import actors.{ EmailHandler, SessionHandler }
import actors.SessionHandler.Session
import akka.actor.{ Actor, Props }
import controllers.Permissions
import org.joda.time.DateTime
import org.scalatestplus.play.OneAppPerSuite
import play.api.libs.concurrent.Akka
import play.api.test.FakeApplication
import play.api.{ Application, GlobalSettings }

trait LWMFakeApplication { self: OneAppPerSuite ⇒
  lazy val fakeGlobals = new GlobalSettings {

    override def onStart(app: Application) {
      import play.api.Play.current

      Akka.system.actorOf(Props(new Actor {
        val fakeAdminSession = Session("fakeAdmin", "fakeAdmin", Permissions.AdminRole)
        val fakeStudentSession = Session("fakeStudent", "fakeStudent", Permissions.DefaultRole)

        override def receive: Receive = {
          case SessionHandler.AuthenticationRequest(user, password) ⇒
            user match {
              case "fakeAdmin"   ⇒ sender() ! fakeAdminSession
              case "fakeStudent" ⇒ sender() ! fakeStudentSession
            }
          case SessionHandler.LogoutRequest(user) ⇒
          case SessionHandler.NameRequest(user) ⇒
            sender() ! ("Fake", "User")
          case SessionHandler.SessionRequest(id) ⇒
            id match {
              case "fakeAdmin"   ⇒ sender() ! fakeAdminSession
              case "fakeStudent" ⇒ sender() ! fakeStudentSession
            }
          case SessionHandler.SessionValidationRequest(id) ⇒
            id match {
              case "fakeAdmin"   ⇒ sender() ! SessionHandler.Valid(fakeAdminSession)
              case "fakeStudent" ⇒ sender() ! SessionHandler.Valid(fakeStudentSession)
            }
        }
      }), "sessions")

      Akka.system.actorOf(Props(new Actor {
        override def receive: Receive = {
          case EmailHandler.MessageRequest(count) ⇒
            sender() ! EmailHandler.EmailResponse(Nil)
        }
      }), "emails")
    }
  }

  override implicit lazy val app: FakeApplication = FakeApplication(withGlobal = Some(fakeGlobals), additionalConfiguration = Map("akka.loglevel" -> "OFF", "logger.play" -> "OFF"))
}
