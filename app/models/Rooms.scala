package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.{QueryHost, UpdateHost}
import utils.semantic._

import scala.concurrent.{ Promise, Future, blocking}

case class Room(roomId: String, name: String, id: UUID = UUID.randomUUID())
case class RoomFormModel(roomId: String, name: String)

object Rooms {
  object Forms {
    val roomForm = Form(
      mapping(
        "id" -> nonEmptyText,
        "name" -> nonEmptyText
      )(RoomFormModel.apply)(RoomFormModel.unapply)
    )
  }

  import utils.Global._
  import utils.semantic.Vocabulary._
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(room: Room)(implicit updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}rooms/${room.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
         |${Vocabulary.defaulPrefixes}
         |
         |Insert data {
         |$resource rdf:type lwm:Room .
         |$resource rdfs:label "${room.name}" .
         |$resource lwm:hasName "${room.name}" .
         |$resource lwm:hasId "${room.id}" .
         |$resource lwm:hasRoomId "${room.roomId}"
         |}
       """.stripMargin
      p.success(resource)
    }
    p.future
  }

  def delete(roomId: String)(implicit queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}rooms/$roomId")

    val p = Promise[Resource]()

    blocking {
      SPARQLBuilder.removeIndividual(resource)
      p.success(resource)
    }

    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Room)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(student ⇒ Individual(student.s)).toList
    }
  }
}

