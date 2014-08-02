package models

import utils.semantic._

import scala.concurrent.Future


case class Student(
                    id: String,
                    firstname: String, lastname: String,
                    registrationNumber: String,
                    email: String,
                    phone: String, degree: String)




object Students {

  import utils.Global._
  import utils.semantic.Vocabulary._

import scala.concurrent.ExecutionContext.Implicits.global


  def create(student: Student): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Student),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.systemId, Literal(student.id)),
      Statement(resource, FOAF.firstName, Literal(student.firstname)),
      Statement(resource, FOAF.lastName, Literal(student.lastname)),
      Statement(resource, NCO.phoneNumber, Literal(student.phone)),
      Statement(resource, FOAF.mbox, Literal(student.email)),
      Statement(resource, LWM.hasEnrollment, Resource(student.degree)),
      Statement(resource, LWM.registrationId, Literal(student.registrationNumber))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def all(): Future[Seq[Individual]] = Future {
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Student))).map(student => Individual(student.s))
  }

  def exists(uid: String): Boolean = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.systemId} ${Literal(uid).toQueryString}}")
}

