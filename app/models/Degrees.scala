package models

import play.api.data.Form
import play.api.data.Forms._
import utils.{ UpdateHost, QueryHost }

import utils.semantic._

import scala.concurrent.{ Promise, Future, blocking }
import utils.Implicits._

// ex: Course("Wirtschaftsinformatik", "WI")
case class Degree(name: String, id: String)

/**
  * Studiengänge.
  */
object Degrees extends CheckedDelete {

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(degree: Degree)(implicit updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}degrees/${degree.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
       |${Vocabulary.defaulPrefixes}
       |
       |Insert data {
       |
       |      $resource rdf:type lwm:Degree .
       |      $resource lwm:hasId "${degree.id}" .
       |      $resource lwm:hasName "${degree.name}" .
       |      $resource rdfs:label "${degree.name}" .
       |}
     """.stripMargin.execUpdate()
      p.success(resource)
    }

    p.future
  }

  def delete(degreeId: String)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}degrees/$degreeId")
    delete(resource)
  }

  def delete(degree: Degree)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}degrees/${degree.id}")
    delete(resource)
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] = Future {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |Select ?s (rdf:type as ?p) (lwm:Degree as ?o) where {
       |    ?s rdf:type lwm:Degree
       |    optional{?s lwm:hasName ?name}
       |
       |} order by asc(?name)
     """.stripMargin.execSelect().map(qs ⇒ Resource(qs.data("s").toString))
  }

  def exists(degree: Degree)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |ASK {
       |    ?s lwm:hasId "${degree.id}" .
       |    ?s lwm:hasName "${degree.name}" .
       |    ?s rdfs:label "${degree.name}" .
       |}
     """.stripMargin.executeAsk()

  }

  override def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |ASK {
       |    $resource rdf:type lwm:Degree
       |}
     """.stripMargin.executeAsk()
  }

  def size()(implicit queryHost: QueryHost): Int = {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |Select (count(distinct ?s) as ?count) where {
       |    ?s rdf:type lwm:Degree
       |}
     """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }
}

object DegreeForms {
  val degreeForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText
  )(Degree.apply)(Degree.unapply))
}