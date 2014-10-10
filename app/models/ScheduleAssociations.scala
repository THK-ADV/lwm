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

case class ScheduleAssociation(group: Resource, assignmentAssoc: Resource, assignmentDate: LocalDate, dueDate: LocalDate, assignmentDateTimetableEntry: Resource, dueDateTimetableEntry: Resource)

object ScheduleAssociations {
  def create(assignment: ScheduleAssociation): Future[Individual] = {
    val id = UUID.randomUUID()
    val assocResource = ResourceUtils.createResource(lwmNamespace, id)

    val statements = List(
      Statement(assocResource, RDF.typ, LWM.ScheduleAssociation),
      Statement(assocResource, RDF.typ, OWL.NamedIndividual),
      Statement(assocResource, LWM.hasAssignmentDate, DateLiteral(assignment.assignmentDate)),
      Statement(assocResource, LWM.hasDueDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, LWM.hasGroup, assignment.group),
      Statement(assignment.group, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasDueDateTimetableEntry, assignment.dueDateTimetableEntry),
      Statement(assocResource, LWM.hasAssignmentDateTimetableEntry, assignment.assignmentDateTimetableEntry),
      Statement(assocResource, LWM.hasAssignmentAssociation, assignment.assignmentAssoc)
    )

    val query =
      s"""
        | select (${assignment.group} as ?s) (${LWM.hasMember} as ?p) ?o where {
        |  ${assignment.group} ${LWM.hasMember} ?o
        | }
      """.stripMargin

    sparqlExecutionContext.executeQuery(query).map { result ⇒
      val stmnt = SPARQLTools.statementsFromString(result)
      val students = stmnt.map(s ⇒ Statement(s.s, LWM.hasScheduleAssociation, assocResource))
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(students: _*))
    }

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

  def dates(group: Resource, association: Resource): Option[(LocalDate, LocalDate)] = {
    val query1 =
      s"""
        |SELECT ?s (${LWM.hasAssignmentDate} as ?p) ?o where {
        | ${group.toQueryString} ${LWM.hasScheduleAssociation} ?s .
        | ?s ${LWM.hasAssignmentAssociation} ${association.toQueryString} .
        | ?s ${LWM.hasAssignmentDate} ?o .
        |}
      """.stripMargin

    val query2 =
      s"""
        |SELECT ?s (${LWM.hasDueDate} as ?p) ?o where {
        | ${group.toQueryString} ${LWM.hasScheduleAssociation} ?s .
        | ?s ${LWM.hasAssignmentAssociation} ${association.toQueryString} .
        | ?s ${LWM.hasDueDate} ?o .
        |}
      """.stripMargin

    for {
      st1 ← SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query1)).headOption
      st2 ← SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query2)).headOption
      assLit ← st1.o.asLiteral()
      dueLit ← st2.o.asLiteral()
    } yield (LocalDate.parse(assLit.decodedString), LocalDate.parse(dueLit.decodedString))

  }

  def times(group: Resource, association: Resource): Option[(Time, Time)] = {
    val query1 =
      s"""
        |SELECT ?s (${LWM.hasStartTime} as ?p) ?o where {
        | ${group.toQueryString} ${LWM.hasScheduleAssociation} ?sca .
        | ?sca ${LWM.hasAssignmentAssociation} ${association.toQueryString} .
        | ?sca ${LWM.hasAssignmentDateTimetableEntry} ?s .
        | ?s ${LWM.hasStartTime} ?o .
        |}
      """.stripMargin

    val query2 =
      s"""
        |SELECT ?s (${LWM.hasStartTime} as ?p) ?o where {
        | ${group.toQueryString} ${LWM.hasScheduleAssociation} ?sca .
        | ?sca ${LWM.hasAssignmentAssociation} ${association.toQueryString} .
        | ?sca ${LWM.hasDueDateTimetableEntry} ?s .
        | ?s ${LWM.hasStartTime} ?o .
        |}
      """.stripMargin

    for {
      st1 ← SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query1)).headOption
      st2 ← SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query2)).headOption
      startTime ← st1.o.asLiteral()
      st = startTime.decodedString.split(":")
      h1 = st(0).toInt
      m1 = st(1).toInt
      endTime ← st2.o.asLiteral()
      et = endTime.decodedString.split(":")
      h2 = et(0).toInt
      m2 = et(1).toInt
    } yield (Time(h1, m1), Time(h2, m2))

  }

  def getSupervisorsFor(scheduleAssociation: Resource): List[Resource] = {
    val query =
      s"""
         |select ($scheduleAssociation as ?s) (${LWM.hasSupervisor} as ?p) (?supervisor as ?o) where{
         | $scheduleAssociation ${LWM.hasAssignmentDateTimetableEntry} ?entry .
         | ?entry ${LWM.hasSupervisor} ?supervisor .
         |}
       """.stripMargin

    val result = sparqlExecutionContext.executeQueryBlocking(query)
    SPARQLTools.statementsFromString(result).map { statement ⇒
      Resource(statement.o.value)
    }.toList
  }
}
