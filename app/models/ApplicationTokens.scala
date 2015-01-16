package models

import java.util.UUID

import utils.Global._
import utils.semantic.Vocabulary.{ owl, lwm, rdf }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class ApplicationToken(student: Resource, labwork: Resource)

object ApplicationTokens {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(token: ApplicationToken): Future[Individual] = {
    val id = UUID.randomUUID()
    val tokenResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(tokenResource, rdf.typ, lwm.ApplicationToken),
      Statement(tokenResource, rdf.typ, owl.NamedIndividual),
      Statement(token.student, lwm.hasApplicationToken, tokenResource),
      Statement(tokenResource, lwm.hasId, StringLiteral(id.toString)),
      Statement(tokenResource, lwm.hasLabWork, token.labwork)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(tokenResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.ApplicationToken)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an ApplicationToken"))
    }
    p.future
  }
}
