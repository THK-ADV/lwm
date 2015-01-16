package models

import java.util.UUID

import utils.Global._
import utils.semantic.Vocabulary.{ owl, lwm, rdf }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class LabworkApplicationList(labwork: Resource, id: UUID = UUID.randomUUID())

object LabworkApplicationLists {

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(application: LabworkApplicationList): Future[Individual] = {
    val listResource = ResourceUtils.createResource(lwmNamespace, application.id)
    val statements = List(
      Statement(listResource, rdf.typ, lwm.LabworkApplicationList),
      Statement(listResource, rdf.typ, owl.NamedIndividual),
      Statement(listResource, lwm.hasId, StringLiteral(application.id.toString)),
      Statement(listResource, lwm.hasLabWork, application.labwork),
      Statement(application.labwork, lwm.hasApplicationList, listResource)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(listResource))
  }

  def delete(applicationList: LabworkApplicationList): Future[LabworkApplicationList] = {
    val maybeApplicationList = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.LabworkApplicationList, Vocabulary.lwm.hasId, StringLiteral(applicationList.id.toString))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeApplicationList)
    val p = Promise[LabworkApplicationList]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(course ⇒ course.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(applicationList) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.LabworkApplicationList)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a LabworkApplicationList"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.LabworkApplicationList)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labworkApplicationList ⇒ Individual(labworkApplicationList.s)).toList
    }
  }

  def getAllApplications(applicationList: Resource): Future[List[Individual]] = {
    val query = s"""
        |select ($applicationList as ?s) (${lwm.hasApplication} as ?p) ?o where{
        |$applicationList ${lwm.hasApplication} ?o
        |}
      """.stripMargin

    sparqlExecutionContext.executeQuery(query).map { result ⇒
      SPARQLTools.statementsFromString(result).map(application ⇒ Individual(Resource(application.o.value))).toList
    }

  }
}
