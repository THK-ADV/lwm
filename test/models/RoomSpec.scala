package models

import base.SemanticFeatureSpec
import utils.semantic.{ Vocabulary, Resource }
import utils.Implicits._
import scala.concurrent.Future

class RoomSpec extends SemanticFeatureSpec {

  import scala.concurrent.ExecutionContext.Implicits.global

  val room_1 = Room("1", "test_room_1")
  val room_2 = Room("2", "test_room_2")

  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/rooms/1")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/rooms/2")

  "Rooms" should {
    "return the number of rooms" in {
      val expected = 0
      eventually {
        Rooms.size should be(expected)
      }
    }

    "create a new room" in {
      val futureRoom = Rooms.create(room_1)
      whenReady(futureRoom) { room ⇒
        room should be(expectedResource1)
        Rooms.size should be(1)

        s"""
          ${Vocabulary.defaulPrefixes}
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:Room .
          |    $expectedResource1 lwm:hasId ?id  .
          |    $expectedResource1 lwm:hasRoomId ?roomId .
          |    $expectedResource1 lwm:hasName ?name .
          |    $expectedResource1 rdfs:label ?label .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val name = solution.data("name").asLiteral().getString
          val roomId = solution.data("roomId").asLiteral().getString
          val label = solution.data("label").asLiteral().getString
          (id, name, roomId, label)
        } should contain theSameElementsAs List((room_1.id, room_1.name, room_1.roomId, room_1.name))
      }
    }

    "delete an existing room" in {
      val futureResource = Rooms.delete(room_1.roomId)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Rooms.size should be(0)
      }
    }

    "return a list of all rooms" in {
      "drop all".execUpdate()
      val futureRooms = Rooms.create(room_1) :: Rooms.create(room_2) :: Nil

      whenReady(Future.sequence(futureRooms)) { rooms ⇒
        whenReady(Rooms.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2)
        }
      }
    }

    "should return true if an arbitrary resource is really a room" in {
      Rooms.check(expectedResource1) should be(true)
    }
  }
}
