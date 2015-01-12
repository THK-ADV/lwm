package models

import play.api.data.Form
import play.api.data.Forms._
import utils.QueryHost

import utils.semantic._

import scala.concurrent.{Promise, Future, blocking}

// ex: Course("Wirtschaftsinformatik", "WI")
case class Degree(name: String, id: String)

/**
 * Studiengänge.
 */
object Degrees {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global


  def create(degree: Degree)(implicit queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}degrees/${degree.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
         |${Vocabulary.defaulPrefixes}
         |
         |$resource rdf:type lwm:Degree .
         |$resource lwm:hasId "${degree.id}" .
         |$resource rdfs:label "${degree.name}" .
         |$resource lwm:hasName "${degree.name}"
       """.stripMargin
      p.success(resource)

      p.future
    }
  }

  def deleteNew(degreeId: String)(implicit queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}degrees/$degreeId")

    val p = Promise[Resource]()

    blocking {

      SPARQLBuilder.removeIndividual(resource)
      p.success(resource)
      p.future
    }

  }

  def delete(degree: Degree): Future[Degree] = {
    val maybeDegree = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.Course, Vocabulary.lwm.hasId, StringLiteral(degree.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeDegree)
    val p = Promise[Degree]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(degree ⇒ degree.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(degree)}
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.Degree)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource)}
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Degree"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Degree)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(degree ⇒ Individual(degree.s)).toList
    }
  }

  def exists(degree: Degree): Future[Boolean] = {
    val aFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.lwm.hasId} ${StringLiteral(degree.id).toQueryString}}")
    val bFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.lwm.hasName} ${StringLiteral(degree.name).toQueryString}}")
    val cFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.rdfs.label} ${StringLiteral(degree.name).toQueryString}}")
    for {
      a ← aFut
      b ← bFut
      c ← cFut
    } yield a || b || c
  }
}

object DegreeForms {
  val degreeForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText
  )(Degree.apply)(Degree.unapply))
}
