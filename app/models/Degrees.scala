package models

import play.api.data.Form
import play.api.data.Forms._

import utils.semantic._

import scala.concurrent.{Promise, Future}


// ex: Course("Wirtschaftsinformatik", "WI")
case class Degree(name: String, id: String)

/**
 * Studiengänge.
 */
object Degrees {
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(degree: Degree): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Degree),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(degree.id)),
      Statement(resource, RDFS.label, Literal(degree.name)),
      Statement(resource, LWM.hasName, Literal(degree.name))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*)).map{ b =>
      Individual(resource)
    }

  }

  def delete(degree: Degree): Future[Degree] = {
    val maybeDegree = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Course, Vocabulary.LWM.hasId, Literal(degree.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeDegree)
    val p = Promise[Degree]()
    resultFuture.map{result =>
      val resources = SPARQLTools.statementsFromString(result).map(degree => degree.s)
      resources.map{resource =>
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{_ => p.success(degree)}
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] =  {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.Degree)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{b => p.success(resource)}
    }else{
      p.failure(new IllegalArgumentException("Resource is not a Degree"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Degree)).map{stringResult =>
      SPARQLTools.statementsFromString(stringResult).map(degree => Individual(degree.s)).toList
    }
  }

  def exists(degree: Degree): Future[Boolean] = {
    val aFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasId} ${Literal(degree.id).toQueryString}}")
    val bFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasName} ${Literal(degree.name).toQueryString}}")
    val cFut = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.RDFS.label} ${Literal(degree.name).toQueryString}}")
    for{
      a <- aFut
      b <- bFut
      c <- cFut
    } yield a || b || c
  }
}

object DegreeForms{
  val degreeForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText
  )(Degree.apply)(Degree.unapply))
}
