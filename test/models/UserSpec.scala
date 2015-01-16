package models

import base.SemanticFeatureSpec
import utils.semantic.Resource

import scala.concurrent.Future

class UserSpec extends SemanticFeatureSpec {
  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.Implicits._

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

        s"""
          |prefix lwm: <http://lwm.gm.fh-koeln.de/>
          |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |prefix owl: <http://www.w3.org/2002/07/owl#>
          |prefix foaf: <http://xmlns.com/foaf/0.1/>
          |prefix nco: <http://www.semanticdesktop.org/ontologies/nco#>
          |
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:User .
          |    $expectedResource1 lwm:hasGmId ?id  .
          |    $expectedResource1 foaf:lastName ?lastname .
          |    $expectedResource1 foaf:firstName ?firstname .
          |    $expectedResource1 rdfs:label ?label .
          |    $expectedResource1 nco:phoneNumber ?phone .
          |    $expectedResource1 foaf:mbox ?email .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val lastname = solution.data("lastname").asLiteral().getString
          val firstname = solution.data("firstname").asLiteral().getString
          val label = solution.data("label").asLiteral().getString
          val phone = solution.data("phone").asLiteral().getString
          val email = solution.data("email").asLiteral().getString
          (id, firstname, lastname, label, phone, email)
        } should contain theSameElementsAs List((user1.id, user1.firstname, user1.lastname, s"${user1.firstname} ${user1.lastname}", user1.phone, user1.email))
      }
    }

    "should return true if an arbitrary resource is really a user" in {
      Users.check(expectedResource1) should be(true)
    }

    "delete an existing user" in {
      val futureResource = Users.delete(user1.id)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Users.size should be(0)
      }
    }

    "return a list of all users" in {
      "drop all".execUpdate()
      val futureUsers = Users.create(user1) ::
        Users.create(user2) ::
        Users.create(user3) :: Nil

      whenReady(Future.sequence(futureUsers)) { users ⇒
        whenReady(Users.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "return a map of possible substitutes and their names" in {
      "drop all".execUpdate()
      val futureUsers = Users.create(user1) ::
        Users.create(user2) ::
        Users.create(user3) :: Nil

      whenReady(Future.sequence(futureUsers)) { users ⇒
        Users.possibleSubstitutes(user1.id) should contain theSameElementsAs List(
          expectedResource2.value -> s"${user2.firstname} ${user2.lastname}",
          expectedResource3.value -> s"${user3.firstname} ${user3.lastname}")
      }
    }

    "return a map of all users and their names" in {
      "drop all".execUpdate()
      val futureUsers = Users.create(user1) ::
        Users.create(user2) ::
        Users.create(user3) :: Nil

      whenReady(Future.sequence(futureUsers)) { users ⇒
        Users.userMapping should contain theSameElementsAs List(
          expectedResource1.value -> s"${user1.firstname} ${user1.lastname}",
          expectedResource2.value -> s"${user2.firstname} ${user2.lastname}",
          expectedResource3.value -> s"${user3.firstname} ${user3.lastname}")
      }
    }
  }

}
