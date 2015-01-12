package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.concurrent.Promise
import utils.semantic._

import scala.concurrent.Future

// Ex: Course("Algorithmen und Programmierung I", "AP1", "http://gm.fh-koeln.de/../Medieninformatik")
case class Course(name: String, id: String, degree: Resource)
case class CourseFormModel(name: String, id: String, degree: String)
/**
  * Veranstaltungen
  */
object Courses {
  import utils.Global._
  import utils.semantic.Vocabulary._
  import scala.concurrent.Promise
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(course: Course): Future[Individual] = {
    val courseResource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(courseResource, rdf.typ, lwm.Course),
      Statement(courseResource, rdf.typ, owl.NamedIndividual),
      Statement(courseResource, lwm.hasId, StringLiteral(course.id)),
      Statement(courseResource, rdfs.label, StringLiteral(course.name)),
      Statement(courseResource, lwm.hasName, StringLiteral(course.name)),
      Statement(courseResource, lwm.hasDegree, course.degree)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(courseResource))
  }

  def delete(course: Course): Future[Course] = {
    val maybeCourse = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.Course, Vocabulary.lwm.hasId, StringLiteral(course.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeCourse)
    val p = Promise[Course]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(course ⇒ course.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(course) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.Course)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Course"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Course)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def exists(course: Course): Future[Boolean] = {
    val a = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.lwm.hasId} ${StringLiteral(course.id).toQueryString}}")
    val b = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.lwm.hasName} ${StringLiteral(course.name).toQueryString}}")
    for {
      aRes ← a
      bRes ← b
    } yield aRes || bRes
  }
}

object CourseForms {
  val courseForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText,
    "degree" -> nonEmptyText
  )(CourseFormModel.apply)(CourseFormModel.unapply))
}

