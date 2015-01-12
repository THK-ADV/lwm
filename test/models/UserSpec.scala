package models

import base.SemanticFeatureSpec
import common.utils.FusekiRunner
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.{ ScalaFutures, IntegrationPatience, Eventually }
import utils.semantic.Resource
import utils.{ QueryHost, UpdateHost }

class UserSpec extends SemanticFeatureSpec {

  val user = User("test_id", "test_firstname", "test_lastname", "test_email", "test_phone")
  val expectedResource = Resource("http://lwm.gm.fh-koeln.de/users/test_id")

  "Users" should {
    "return the number of users" in {
      val expected = 0
      eventually {
        Users.size should be(expected)
      }
    }

    "create a new user" in {
      val futureUser = Users.create(user)
      whenReady(futureUser) { user ⇒
        user should be(expectedResource)
        Users.size should be(1)
      }
    }

    "delete an existing user" in {
      val futureResource = Users.deleteNew(user.id)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource)
        Users.size should be(0)
      }
    }
  }

}
