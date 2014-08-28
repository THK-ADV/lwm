package models

import play.api.data.Form
import play.api.data.Forms._
import utils.semantic._

import scala.concurrent.Future


// Ex: Course("Algorithmen und Programmierung I, "AP1")
case class Course(name: String, id: String)

/**
 * Veranstaltungen
 */
object Courses {
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(course: Course): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Course),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(course.id)),
      Statement(resource, RDFS.label, Literal(course.name)),
      Statement(resource, LWM.hasName, Literal(course.name))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def delete(course: Course) = {
    val maybeCourse = SPARQLBuilder.listIndividualsWithProperty(Vocabulary.LWM.hasId, Literal(course.id))
    val courseResource = SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(maybeCourse)).map(course => course.s)
    courseResource.map(res => sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(res, lwmGraph)))
  }
  def delete(resource: Resource): Unit = {
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.Course)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph))
    }
  }
  def all() = Future{
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Course))).map(course => Individual(course.s))
  }
}

object CourseForms{
  val courseForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText
  )(Course.apply)(Course.unapply))
}

