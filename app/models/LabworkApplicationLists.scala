package models

import java.util.UUID

import utils.Global._
import utils.semantic.Vocabulary.{ OWL, LWM, RDF }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class LabworkApplicationList(labwork: Resource, id: UUID = UUID.randomUUID())

object LabworkApplicationLists {

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(application: LabworkApplicationList): Future[Individual] = {
    val listResource = ResourceUtils.createResource(lwmNamespace, application.id)
    val statements = List(
      Statement(listResource, RDF.typ, LWM.LabworkApplicationList),
      Statement(listResource, RDF.typ, OWL.NamedIndividual),
      Statement(listResource, LWM.hasId, StringLiteral(application.id.toString)),
      Statement(listResource, LWM.hasLabWork, application.labwork),
      Statement(application.labwork, LWM.hasApplicationList, listResource)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(listResource))
  }

  def delete(applicationList: LabworkApplicationList): Future[LabworkApplicationList] = {
    val maybeApplicationList = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.LabworkApplicationList, Vocabulary.LWM.hasId, StringLiteral(applicationList.id.toString))
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
    if (individual.props(RDF.typ).contains(LWM.LabworkApplicationList)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a LabworkApplicationList"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LabworkApplicationList)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labworkApplicationList ⇒ Individual(labworkApplicationList.s)).toList
    }
  }
}
