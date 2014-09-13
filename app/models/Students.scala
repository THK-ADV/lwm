package models

import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class Student(
  gmId: String,
  firstname: String, lastname: String,
  registrationNumber: String,
  email: String,
  phone: String, degree: String)

object Students {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(student: Student): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Student),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasGmId, Literal(student.gmId)),
      Statement(resource, FOAF.firstName, Literal(student.firstname)),
      Statement(resource, FOAF.lastName, Literal(student.lastname)),
      Statement(resource, RDFS.label, Literal(s"${student.firstname} ${student.lastname}")),
      Statement(resource, NCO.phoneNumber, Literal(student.phone)),
      Statement(resource, FOAF.mbox, Literal(student.email)),
      Statement(resource, LWM.hasEnrollment, Resource(student.degree)),
      Statement(resource, LWM.registrationId, Literal(student.registrationNumber))
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(student: Student): Future[Student] = {
    val maybeStudent = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Student, Vocabulary.LWM.hasGmId, Literal(student.gmId))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeStudent)
    val p = Promise[Student]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(student ⇒ student.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map { _ ⇒ p.success(student) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Student)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Student"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Student)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(student ⇒ Individual(student.s)).toList
    }
  }

  def exists(uid: String): Future[Boolean] = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasGmId} ${Literal(uid).toQueryString}}")
}

