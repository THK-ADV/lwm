import play.api.test.Helpers._
import play.api.test._


class ApplicationSpec extends LWMBaseSpec{
  "Application Controller" should {
    "redirect to Admin Dashboard with a valid admin session" in {
      val result = route(FakeRequest(GET, "/").withSession("session" -> "fakeAdmin")).get
      redirectLocation(result) mustBe Some("/administration/dashboard")
    }

    "redirect to Student Dashboard with a valid student session" in {
      val result = route(FakeRequest(GET, "/").withSession("session" -> "fakeStudent")).get
      redirectLocation(result) mustBe Some("/student/dashboard")
    }

    "redirect to Login when there is no valid session" in {
      val result = route(FakeRequest(GET, "/")).get
      redirectLocation(result) mustBe Some("/login")
    }
  }
}
