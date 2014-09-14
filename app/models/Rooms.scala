package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.semantic._

import scala.concurrent.{ Promise, Future }

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

  def create(room: Room): Future[Individual] = {
    val courseResource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(courseResource, RDF.typ, LWM.Room),
      Statement(courseResource, RDF.typ, OWL.NamedIndividual),
      Statement(courseResource, LWM.hasName, Literal(room.name)),
      Statement(courseResource, RDFS.label, Literal(room.name)),
      Statement(courseResource, LWM.hasId, Literal(room.id.toString)),
      Statement(courseResource, LWM.hasRoomId, Literal(room.roomId))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(_ ⇒ Individual(courseResource))
  }

  def delete(room: Room): Future[Room] = {
    val maybeRoom = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Room, Vocabulary.LWM.hasId, Literal(room.id.toString))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeRoom)
    val p = Promise[Room]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(room) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Room)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Room"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Room)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(student ⇒ Individual(student.s)).toList
    }
  }
}

