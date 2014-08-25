import play.api.test.Helpers._
import play.api.test._



class AdminDashboardSpec extends LWMBaseSpec{

  "AdminDashboard" should {
    "redirect to default index if user does not have the right permissions" in  {
      val result = route(FakeRequest(GET, "/administration/dashboard")).get
      redirectLocation(result) mustBe Some("/")
    }

    "return Http 200 Ok if permissions are correct" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeAdmin")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
    }

    "contain a link to Studentenverwaltung" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeAdmin")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
      contentAsString(result).contains("Studentenverwaltung")
    }

    "contain a link to Nutzerverwaltung" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeAdmin")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
      contentAsString(result).contains("Nutzerverwaltung")
    }

    "contain a link to Praktikumsverwaltung" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeAdmin")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
      contentAsString(result).contains("Nutzerverwaltung")
    }

    "contain a link to Studiengangsverwaltung" in {
      val result = route(FakeRequest(GET, "/administration/dashboard").withSession("session" -> "fakeAdmin")).get
      contentType(result) mustBe Some("text/html")
      status(result) mustEqual OK
      contentAsString(result).contains("Nutzerverwaltung")
    }
  }

}
