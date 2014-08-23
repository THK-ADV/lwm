import actors.SessionHandler.Session
import actors.{EmailHandler, SessionHandler}
import akka.actor.{Actor, Props}
import controllers.Permissions
import org.joda.time.DateTime
import org.scalatestplus.play._
import play.api.libs.concurrent.Akka
import play.api.mvc.Results
import play.api.test.Helpers._
import play.api.test._
import play.api.{Application, GlobalSettings}



class AdminDashboardSpec extends PlaySpec with Results with OneAppPerSuite {


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


  "AdminDashboard#index" should {
    "redirect to default index if user does not have the right permissions" in  {
      val result = route(FakeRequest(GET, "/administration/dashboard")).get
      headers(result).apply("Location") mustBe "/"
    }

    "return Http 200 Ok if permissions are correct and admin dashboard is displayed" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeId")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
    }
  }

}
