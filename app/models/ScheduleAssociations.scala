package models

import java.net.URLDecoder
import java.util.UUID

import com.hp.hpl.jena.query.{ QuerySolution, QueryExecutionFactory }
import org.joda.time.{ LocalTime, LocalDate }
import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ lwm, owl, rdf, rdfs }
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
      Statement(assocResource, rdf.typ, lwm.ScheduleAssociation),
      Statement(assocResource, rdf.typ, owl.NamedIndividual),
      Statement(assocResource, lwm.hasAssignmentDate, DateLiteral(assignment.assignmentDate)),
      Statement(assignment.timetable, lwm.hasScheduleAssociation, assocResource),
      Statement(assocResource, lwm.hasDueDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, lwm.hasGroup, assignment.group),
      Statement(assignment.group, lwm.hasScheduleAssociation, assocResource),
      Statement(assocResource, lwm.hasDueDateTimetableEntry, assignment.dueDateTimetableEntry),
      Statement(assocResource, lwm.hasAssignmentDateTimetableEntry, assignment.assignmentDateTimetableEntry),
      Statement(assocResource, lwm.hasAssignmentAssociation, assignment.assignmentAssoc)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(assocResource))
  }

  def create(assignment: ScheduleAssociation, student: Resource): Future[Individual] = {
    val id = UUID.randomUUID()
    val assocResource = ResourceUtils.createResource(lwmNamespace, id)

    val statements = List(
      Statement(assocResource, rdf.typ, lwm.ScheduleAssociation),
      Statement(assocResource, rdf.typ, owl.NamedIndividual),
      Statement(assocResource, lwm.hasAssignmentDate, DateLiteral(assignment.assignmentDate)),
      Statement(assignment.timetable, lwm.hasScheduleAssociation, assocResource),
      Statement(assocResource, lwm.hasDueDate, DateLiteral(assignment.dueDate)),
      Statement(assocResource, lwm.hasGroup, assignment.group),
      Statement(student, lwm.hasScheduleAssociation, assocResource),
      Statement(assocResource, lwm.hasDueDateTimetableEntry, assignment.dueDateTimetableEntry),
      Statement(assocResource, lwm.hasAssignmentDateTimetableEntry, assignment.assignmentDateTimetableEntry),
      Statement(assocResource, lwm.hasAssignmentAssociation, assignment.assignmentAssoc)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(assocResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.ScheduleAssociation)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an ScheduleAssociation"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.ScheduleAssociation)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def dates(group: Resource, association: Resource): Option[(LocalDate, LocalDate)] = {
    val query1 =
      s"""
        |SELECT ?s (${lwm.hasAssignmentDate} as ?p) ?o where {
        | ${group.toQueryString} ${lwm.hasScheduleAssociation} ?s .
        | ?s ${lwm.hasAssignmentAssociation} ${association.toQueryString} .
        | ?s ${lwm.hasAssignmentDate} ?o .
        |}
      """.stripMargin

    val query2 =
      s"""
        |SELECT ?s (${lwm.hasDueDate} as ?p) ?o where {
        | ${group.toQueryString} ${lwm.hasScheduleAssociation} ?s .
        | ?s ${lwm.hasAssignmentAssociation} ${association.toQueryString} .
        | ?s ${lwm.hasDueDate} ?o .
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
        |SELECT ?s (${lwm.hasStartTime} as ?p) ?o where {
        | ${group.toQueryString} ${lwm.hasScheduleAssociation} ?sca .
        | ?sca ${lwm.hasAssignmentAssociation} ${association.toQueryString} .
        | ?sca ${lwm.hasAssignmentDateTimetableEntry} ?s .
        | ?s ${lwm.hasStartTime} ?o .
        |}
      """.stripMargin

    val query2 =
      s"""
        |SELECT ?s (${lwm.hasStartTime} as ?p) ?o where {
        | ${group.toQueryString} ${lwm.hasScheduleAssociation} ?sca .
        | ?sca ${lwm.hasAssignmentAssociation} ${association.toQueryString} .
        | ?sca ${lwm.hasDueDateTimetableEntry} ?s .
        | ?s ${lwm.hasStartTime} ?o .
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
         |select ($scheduleAssociation as ?s) (${lwm.hasSupervisor} as ?p) (?supervisor as ?o) where{
         | $scheduleAssociation ${lwm.hasAssignmentDateTimetableEntry} ?entry .
         | ?entry ${lwm.hasSupervisor} ?supervisor .
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
         |  $scheduleAssociation ${lwm.hasAssignmentDate} ?assignmentDate .
         |  $scheduleAssociation ${lwm.hasDueDate} ?dueDate .
         |  $scheduleAssociation ${lwm.hasAssignmentDateTimetableEntry} ?assignmentDateEntry .
         |  $scheduleAssociation ${lwm.hasDueDateTimetableEntry} ?dueDateEntry .
         |  $scheduleAssociation ${lwm.hasAssignmentDateTimetableEntry} ?assignmentEntry .
         |  $scheduleAssociation ${lwm.hasAssignmentAssociation} ?assignmentAssociation .
         |  ?timetable ${rdf.typ} ${lwm.Timetable} .
         |  ?timetable ${lwm.hasScheduleAssociation} $scheduleAssociation
         |}
       """.stripMargin

    val t = Individual(group).props.get(lwm.hasScheduleAssociation).map { schedules ⇒
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
    val results = QueryExecutionFactory.sparqlService(queryHost, query).execSelect()
    var alternates = List.empty[(String, String)]

    while (results.hasNext) {
      val solution = results.nextSolution()
      val altSchedule = solution.getResource("association").getURI
      val altGroupId = solution.getLiteral("groupId").getString
      val altDate = solution.getLiteral("date").getString
      val altTime = URLDecoder.decode(solution.getLiteral("time").getString, "UTF-8")
      val groupMembersSize = {
        val newGroup = Resource(solution.getResource("group").getURI)
        val groupCount = Individual(newGroup).props.getOrElse(lwm.hasMember, List(StringLiteral("")))
        val normalizedGroupCount = 0 //getNormalizedCount(newGroup, altDate)
        groupCount.size + normalizedGroupCount
      }
      alternates = (altSchedule, s"$altDate, $altTime Gruppe $altGroupId ($groupMembersSize)") :: alternates
    }

    alternates
  }

  def getNormalizedCount(group: Resource, date: String): Int = {
    val start = System.nanoTime()
    import utils.Implicits._
    val queryAlternate =
      s"""
          PREFIX owl: <http://www.w3.org/2002/07/owl#>
          PREFIX lwm: <http://lwm.gm.fh-koeln.de/>
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        Select ?s where {
          ?s lwm:memberOf $group .
          ?group lwm:hasLabWork ?labwork .
          ?s lwm:hasScheduleAssociation ?sched .
          ?sched lwm:hasAlternateScheduleAssociation ?alter .
          ?alter lwm:hasAssignmentDate "$date" .
          ?alter lwm:hasGroup ?g2 .
          ?g2 lwm:hasLabWork ?labwork
        }
      """.stripMargin.execSelect().size

    val duration = (System.nanoTime() - start) / 1000000
    println(duration)
    val queryHidden =
      s"""
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        PREFIX lwm: <http://lwm.gm.fh-koeln.de/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

       Select ?s where {
        ?s lwm:memberOf $group .
        ?group lwm:hasLabWork ?labwork .
        ?s lwm:hasHidingState ?state .
        ?state lwm:hasHidingSubject ?labwork
    }
     """.stripMargin.execSelect().size

    queryAlternate - queryHidden
  }
}
