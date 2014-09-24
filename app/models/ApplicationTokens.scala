package models

import java.util.UUID

import utils.Global._
import utils.semantic.Vocabulary.{ OWL, LWM, RDF }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class ApplicationToken(student: Resource, labwork: Resource)

object ApplicationTokens {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(token: ApplicationToken): Future[Individual] = {
    val id = UUID.randomUUID()
    val tokenResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(tokenResource, RDF.typ, LWM.ApplicationToken),
      Statement(tokenResource, RDF.typ, OWL.NamedIndividual),
      Statement(token.student, LWM.hasApplicationToken, tokenResource),
      Statement(tokenResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(tokenResource, LWM.hasLabWork, token.labwork)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(tokenResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.ApplicationToken)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an ApplicationToken"))
    }
    p.future
  }
}
