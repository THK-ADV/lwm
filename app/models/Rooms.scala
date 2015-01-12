package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.{ QuerySolution, QueryHost, UpdateHost }
import utils.semantic._
import utils.Implicits._
import scala.concurrent.{ Promise, Future, blocking }

case class Room(roomId: String, name: String, id: UUID = UUID.randomUUID())
case class RoomFormModel(roomId: String, name: String)

object Rooms extends CheckedDelete {

  object Forms {
    val roomForm = Form(
      mapping(
        "id" -> nonEmptyText,
        "name" -> nonEmptyText
      )(RoomFormModel.apply)(RoomFormModel.unapply)
    )
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(room: Room)(implicit updateHost: UpdateHost): Future[Resource] = {
    import utils.Global._

    val resource = Resource(s"${lwmNamespace}rooms/${room.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
         ${Vocabulary.defaulPrefixes}
         |insert data {
         |$resource rdf:type lwm:Room .
         |$resource rdfs:label "${room.name}" .
         |$resource lwm:hasName "${room.name}" .
         |$resource lwm:hasId "${room.id}" .
         |$resource lwm:hasRoomId "${room.roomId}" .
         |}
       """.stripMargin.execUpdate()
      p.success(resource)
    }
    p.future
  }

  def delete(roomId: String)(implicit queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}rooms/$roomId")
    delete(resource)
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] =
    Future {
      s"""
        ${Vocabulary.defaulPrefixes}
       Select ?s (rdf:type as ?p) (lwm:Room as ?o) {
       ?s rdf:type lwm:Room
       }
     """.stripMargin.execSelect().map(s ⇒ Resource(s.data("s").toString))
    }

  override def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
         ${Vocabulary.defaulPrefixes}
         |
         |ASK {
         | $resource rdf:type lwm:Room
         |}
       """.stripMargin.executeAsk()
  }
}

