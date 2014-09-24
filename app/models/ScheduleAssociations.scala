package models

import java.util.UUID

import org.joda.time.{ LocalTime, LocalDate }
import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ LWM, OWL, RDF, RDFS }
import utils.semantic._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }

case class ScheduleAssociation(group: Resource, assignmentAssoc: Resource, assignmentDate: LocalDate, dueDate: LocalDate)

object ScheduleAssociations {
  def create(assignment: ScheduleAssociation): Future[Individual] = {
    val id = UUID.randomUUID()
    val assocResource = ResourceUtils.createResource(lwmNamespace, id)

    val statements = List(
      Statement(assocResource, RDF.typ, LWM.ScheduleAssociation),
      Statement(assocResource, RDF.typ, OWL.NamedIndividual),
      Statement(assocResource, LWM.hasStartDate, DateLiteral(assignment.assignmentDate)),
      Statement(assocResource, LWM.hasEndDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, LWM.hasGroup, assignment.group),
      Statement(assignment.group, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasAssignmentAssociation, assignment.assignmentAssoc)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(assocResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.ScheduleAssociation)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an ScheduleAssociation"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.ScheduleAssociation)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def dueDate(group: Resource, association: Resource): LocalDate = {
    val query =
      s"""
        |SELECT ?s (${LWM.hasEndDate} as ?p) ?o where {
        | ${group.toQueryString} ${LWM.hasScheduleAssociation.toQueryString} ?s .
        | ?s ${LWM.hasAssignmentAssociation} ${association.toQueryString} .
        | ?s ${LWM.hasEndDate} ?o .
        |}
      """.stripMargin

    LocalDate.parse(SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).head.o.value)
  }
}
