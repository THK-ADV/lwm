package models

import org.joda.time.DateTime
import util.semantic._

import scala.concurrent.Future


case class Student(
                    id: String,
                    firstnames: List[String], lastnames: List[String],
                    registrationNumber: String,
                    email: List[String],
                    phone: String, degree: List[Resource])


object Students {

  import util.Global._
  import Vocabulary._
  import scala.concurrent.ExecutionContext.Implicits.global


  def create(student: Student): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    println(s"Resource: $resource")
    val statements = List(
      Statement(resource, RDF.typ, LWM.Student),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.systemId, Literal(student.id)),
      Statement(resource, NCO.phoneNumber, Literal(student.phone)),
      Statement(resource, LWM.registrationId, Literal(student.registrationNumber))
    ) ::: student.email.map(e => Statement(resource, FOAF.mbox, Literal(e))) :::
      student.firstnames.map(fn => Statement(resource, FOAF.firstName, Literal(fn))) :::
      student.lastnames.map(ln => Statement(resource, FOAF.lastName, Literal(ln))) :::
      student.degree.map(d => Statement(resource, LWM.hasEnrollment, d))

    val response = sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def all(): Future[Seq[Individual]] = Future {

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Student))).map(student => Individual(student.s))
  }

  def retrieve(property: Property, value: RDFNode): Future[Student] = ???

}
