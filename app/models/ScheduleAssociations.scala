package models

import java.net.URLDecoder
import java.util.UUID

import com.hp.hpl.jena.query.{ QuerySolution, QueryExecutionFactory }
import org.joda.time.{ LocalTime, LocalDate }
import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ LWM, OWL, RDF, RDFS }
import utils.semantic._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }

case class ScheduleAssociation(group: Resource, assignmentAssoc: Resource, assignmentDate: LocalDate, dueDate: LocalDate, assignmentDateTimetableEntry: Resource, dueDateTimetableEntry: Resource, timetable: Resource)

case class AlternateAssociationFormModel(oldSchedule: String, newSchedule: String)

object ScheduleAssociations {
  object Forms {
    import play.api.data.Forms._
    import play.api.data._

    val alternateForm = Form(mapping(
      "oldSchedule" -> nonEmptyText,
      "newSchedule" -> nonEmptyText
    )(AlternateAssociationFormModel.apply)(AlternateAssociationFormModel.unapply))
  }

  def create(assignment: ScheduleAssociation): Future[Individual] = {
    val id = UUID.randomUUID()
    val assocResource = ResourceUtils.createResource(lwmNamespace, id)

    val statements = List(
      Statement(assocResource, RDF.typ, LWM.ScheduleAssociation),
      Statement(assocResource, RDF.typ, OWL.NamedIndividual),
      Statement(assocResource, LWM.hasAssignmentDate, DateLiteral(assignment.assignmentDate)),
      Statement(assignment.timetable, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasDueDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, LWM.hasGroup, assignment.group),
      Statement(assignment.group, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasDueDateTimetableEntry, assignment.dueDateTimetableEntry),
      Statement(assocResource, LWM.hasAssignmentDateTimetableEntry, assignment.assignmentDateTimetableEntry),
      Statement(assocResource, LWM.hasAssignmentAssociation, assignment.assignmentAssoc)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(assocResource))
  }

  def create(assignment: ScheduleAssociation, student: Resource): Future[Individual] = {
    val id = UUID.randomUUID()
    val assocResource = ResourceUtils.createResource(lwmNamespace, id)

    val statements = List(
      Statement(assocResource, RDF.typ, LWM.ScheduleAssociation),
      Statement(assocResource, RDF.typ, OWL.NamedIndividual),
      Statement(assocResource, LWM.hasAssignmentDate, DateLiteral(assignment.assignmentDate)),
      Statement(assignment.timetable, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasDueDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, LWM.hasGroup, assignment.group),
      Statement(student, LWM.hasScheduleAssociation, assocResource),
      Statement(assocResource, LWM.hasDueDateTimetableEntry, assignment.dueDateTimetableEntry),
      Statement(assocResource, LWM.hasAssignmentDateTimetableEntry, assignment.assignmentDateTimetableEntry),
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

  def getForGroup(group: Resource): Future[List[ScheduleAssociation]] = Future {

    def query2(scheduleAssociation: Resource) =
      s"""
         |select * where {
         |  $scheduleAssociation ${LWM.hasAssignmentDate} ?assignmentDate .
         |  $scheduleAssociation ${LWM.hasDueDate} ?dueDate .
         |  $scheduleAssociation ${LWM.hasAssignmentDateTimetableEntry} ?assignmentDateEntry .
         |  $scheduleAssociation ${LWM.hasDueDateTimetableEntry} ?dueDateEntry .
         |  $scheduleAssociation ${LWM.hasAssignmentDateTimetableEntry} ?assignmentEntry .
         |  $scheduleAssociation ${LWM.hasAssignmentAssociation} ?assignmentAssociation .
         |  ?timetable ${RDF.typ} ${LWM.Timetable} .
         |  ?timetable ${LWM.hasScheduleAssociation} $scheduleAssociation
         |}
       """.stripMargin

    val t = Individual(group).props.get(LWM.hasScheduleAssociation).map { schedules ⇒
      schedules.map { schedule ⇒
        val q = query2(schedule.asResource().get)
        val result = QueryExecutionFactory.sparqlService(queryHost, q).execSelect()
        var sss = List.empty[ScheduleAssociation]
        while (result.hasNext) {
          val n = result.nextSolution()
          val assignmentDate = LocalDate.parse(n.get("assignmentDate").toString)
          val dueDate = LocalDate.parse(n.get("dueDate").toString)
          val dueDateEntry = Resource(n.get("dueDateEntry").toString)
          val assignmentEntry = Resource(n.get("assignmentEntry").toString)
          val assignmentAssociation = Resource(n.get("assignmentAssociation").toString)
          val timetable = Resource(n.get("timetable").toString)

          sss = ScheduleAssociation(group, assignmentAssociation, assignmentDate, dueDate, assignmentEntry, dueDateEntry, timetable) :: sss
        }
        sss
      }.flatten
    }

    t match {
      case Some(list) ⇒ list
      case None       ⇒ Nil
    }
  }

  def getAlternateDates(oldSchedule: Resource, group: Resource, groupId: String, orderId: String) = {
    val query =
      s"""
         prefix lwm: <http://lwm.gm.fh-koeln.de/>
        prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        select * where {
          ?labwork lwm:hasGroup $group .
          ?labwork lwm:hasGroup ?group .
          ?group lwm:hasScheduleAssociation ?association .
          ?group lwm:hasGroupId ?groupId .
          ?association lwm:hasAssignmentAssociation ?assignmentAssociation .
          ?association lwm:hasAssignmentDate ?date .
          ?association lwm:hasAssignmentDateTimetableEntry ?entry .
          ?entry lwm:hasStartTime ?time .
          ?assignmentAssociation lwm:hasOrderId "$orderId" .
          filter not exists {?group lwm:hasGroupId "$groupId"}
        }order by desc(?date) desc(?time)
       """.stripMargin
    println(query)
    val results = QueryExecutionFactory.sparqlService(queryHost, query).execSelect()
    var alternates = List.empty[(String, String)]
    while (results.hasNext) {
      val solution = results.nextSolution()
      val altSchedule = solution.getResource("association").getURI
      val altGroupId = solution.getLiteral("groupId").getString
      val altDate = solution.getLiteral("date").getString
      val altTime = URLDecoder.decode(solution.getLiteral("time").getString, "UTF-8")
      alternates = (altSchedule, s"$altDate, $altTime Gruppe $altGroupId") :: alternates
    }
    alternates
  }
}
