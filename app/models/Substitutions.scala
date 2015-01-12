package models

import java.util.UUID

import org.joda.time.{ LocalTime, LocalDate }
import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ lwm, owl, rdf, rdfs }
import utils.semantic._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }

case class Substitution(scheduleAssociation: Resource, substitute: Resource)
case class SubstitutionFormModel(scheduleAssociation: String, substitute: String)

object Substitutions {
  object Forms {
    val subsitutionForm = Form(mapping(
      "scheduleAssociation" -> nonEmptyText,
      "substitute" -> nonEmptyText
    )(SubstitutionFormModel.apply)(SubstitutionFormModel.unapply))
  }
  def create(substitution: Substitution): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, rdf.typ, lwm.Substitution),
      Statement(resource, rdf.typ, owl.NamedIndividual),
      Statement(resource, lwm.hasScheduleAssociation, substitution.scheduleAssociation),
      Statement(substitution.scheduleAssociation, lwm.hasSubstitution, resource),
      Statement(resource, lwm.hasSubstitute, substitution.substitute)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { b ⇒
      Individual(resource)
    }
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.Substitution)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Substitution"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Substitution)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(degree ⇒ Individual(degree.s)).toList
    }
  }
}
