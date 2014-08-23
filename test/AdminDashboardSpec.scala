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



class AdminDashboardSpec extends PlaySpec with Results with OneAppPerSuite with LWMFakeApplication{

  "AdminDashboard#index" should {
    "redirect to default index if user does not have the right permissions" in  {
      val result = route(FakeRequest(GET, "/administration/dashboard")).get
      redirectLocation(result) mustBe Some("/")
    }

    "return Http 200 Ok if permissions are correct and admin dashboard is displayed" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeId")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
    }
  }

}
