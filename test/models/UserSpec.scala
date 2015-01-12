package models

import base.SemanticFeatureSpec
import utils.semantic.Resource

import scala.concurrent.Future

class UserSpec extends SemanticFeatureSpec {
  import scala.concurrent.ExecutionContext.Implicits.global

  val user1 = User("test_id_1", "test_firstname", "test_lastname", "test_email", "test_phone")
  val user2 = User("test_id_2", "test_firstname", "test_lastname", "test_email", "test_phone")
  val user3 = User("test_id_3", "test_firstname", "test_lastname", "test_email", "test_phone")
  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/users/test_id_1")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/users/test_id_2")
  val expectedResource3 = Resource("http://lwm.gm.fh-koeln.de/users/test_id_3")

  "Users" should {
    "return the number of users" in {
      val expected = 0
      eventually {
        Users.size should be(expected)
      }
    }

    "create a new user" in {
      val futureUser = Users.create(user1)
      whenReady(futureUser) { user ⇒
        user should be(expectedResource1)
        Users.size should be(1)
      }
    }

    "delete an existing user" in {
      val futureResource = Users.delete(user1.id)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Users.size should be(0)
      }
    }

    "return a list of all users" in {
      import utils.Implicits._
      "drop all".execUpdate()
      val futureUsers = Users.create(user1) ::
        Users.create(user2) ::
        Users.create(user3) :: Nil

      whenReady(Future.sequence(futureUsers)) { users ⇒
        whenReady(Users.all()) { all ⇒
          all.toSet should be(Set(expectedResource1, expectedResource2, expectedResource3))
        }
      }

    }
  }

}
