package models

import play.api.data.Form
import play.api.data.Forms._

import utils.semantic._

import scala.concurrent.Future


// ex: Course("Wirtschaftsinformatik", "WI")
case class Degree(name: String, id: String)

/**
 * Studiengänge.
 */
object Degrees {
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(degree: Degree): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Degree),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(degree.id)),
      Statement(resource, RDFS.label, Literal(degree.name)),
      Statement(resource, LWM.hasName, Literal(degree.name))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def delete(degree: Degree) = {
    val maybeDegree = SPARQLBuilder.listIndividualsWithProperty(Vocabulary.LWM.hasId, Literal(degree.id))
    val degreeResource = SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(maybeDegree)).map(course => course.s)
    degreeResource.map(res => sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(res, lwmGraph)))
  }

  def delete(resource: Resource): Unit = {
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.Degree)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph))
    }
  }

  def all(): Future[Seq[Individual]] = Future{
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Degree))).map(course => Individual(course.s))
  }

  def exists(degree: Degree): Boolean = {
    val a = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasId} ${Literal(degree.id).toQueryString}}")
    val b = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasName} ${Literal(degree.name).toQueryString}}")
    val c = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.RDFS.label} ${Literal(degree.name).toQueryString}}")
    a || b || c
  }
}

object DegreeForms{
  val degreeForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText
  )(Degree.apply)(Degree.unapply))
}
