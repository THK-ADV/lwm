package models

import java.net.URLDecoder
import java.util.Date

import com.hp.hpl.jena.query.QueryExecutionFactory
import org.joda.time.{ LocalDate, DateTime }
import utils.semantic._
import utils.Implicits._
import scala.concurrent.{ Promise, Future }

case class LabWork(course: Resource, semester: Resource)

case class LabWorkFormModel(courseId: String, semester: String)

case class LabworkUpdateModel(courseId: String, semester: String, startDate: Date, endDate: Date)

object LabworkExportModes {
  val PublicSchedule = "publicSchedule"
  val PublicGroupMembersTable = "publicMembers"
  val InternalSchedule = "internalSchedule"
  val AssessmentSchedule = "assessmentSchedule"
  val LabworkGraduates = "labworkGraduates"
}

object LabWorkForms {

  import play.api.data.Forms._
  import play.api.data._

  val labworkForm = Form(
    mapping(
      "courseId" -> nonEmptyText,
      "semester" -> nonEmptyText
    )(LabWorkFormModel.apply)(LabWorkFormModel.unapply)
  )

  val labworkUpdateForm = Form(
    mapping(
      "courseId" -> nonEmptyText,
      "semester" -> nonEmptyText,
      "startDate" -> date,
      "endDate" -> date
    )(LabworkUpdateModel.apply)(LabworkUpdateModel.unapply)
  )
}

/**
  * Praktika
  */
object LabWorks {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(labWork: LabWork): Future[Individual] = {
    val semesterIndividual = Individual(labWork.semester)
    val startDate = semesterIndividual.props.getOrElse(lwm.hasStartDate, List(new DateLiteral(LocalDate.now()))).head
    val endDate = semesterIndividual.props.getOrElse(lwm.hasEndDate, List(new DateLiteral(LocalDate.now()))).head

    val courseIndividual = Individual(labWork.course)

    val resource = ResourceUtils.createResource(lwmNamespace)
    val futureTimetable = Timetables.create(Timetable(resource))
    val labworkApplicationList = LabworkApplicationLists.create(LabworkApplicationList(resource))

    val label = courseIndividual.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.asLiteral().get

    val futureStatements = futureTimetable.map { timetable ⇒
      List(
        Statement(resource, rdf.typ, lwm.LabWork),
        Statement(resource, rdf.typ, owl.NamedIndividual),
        Statement(resource, rdfs.label, label),
        Statement(resource, lwm.hasTimetable, timetable),
        Statement(resource, lwm.hasCourse, labWork.course),
        Statement(resource, lwm.hasStartDate, startDate),
        Statement(resource, lwm.hasEndDate, endDate),
        Statement(resource, lwm.allowsApplications, StringLiteral("false")),
        Statement(resource, lwm.isClosed, StringLiteral("false")),
        Statement(resource, lwm.hasSemester, labWork.semester)
      )
    }

    labworkApplicationList.flatMap { list ⇒
      futureStatements.flatMap { statements ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
          Individual(resource)
        }
      }
    }
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.LabWork)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.LabWork)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }

  def orderedGroups(labwork: Resource) = {
    val query =
      s"""
         |select * where {
         |$labwork ${Vocabulary.lwm.hasGroup} ?group .
         |?group ${Vocabulary.lwm.hasGroupId} ?id .
         |} order by desc(?id)
       """.stripMargin

    val result = QueryExecutionFactory.sparqlService(queryHost, query).execSelect()
    var groups = List.empty[(Resource, String)]
    while (result.hasNext) {
      val n = result.nextSolution()
      val group = Resource(n.getResource("group").toString)
      val id = n.getLiteral("id").getString
      groups = (group, id) :: groups
    }
    groups
  }

  def labworksForDate(date: LocalDate) = {
    println(date.toString("yyyy-MM-dd"))
    val query =
      s"""
        select ?course ?group ?groupId ?startTime ?endTime ?name ?courseName ?roomId ?degreeName ?orderId where {
          ?group ${rdf.typ} ${lwm.Group} .
          ?group ${lwm.hasGroupId} ?groupId .
          ?group ${lwm.hasScheduleAssociation} ?schedule .
          optional {
          ?schedule ${lwm.hasAssignmentAssociation} ?assignmentAssociation .
          ?assignmentAssociation ${lwm.hasOrderId} ?orderId .
          }
          ?schedule ${lwm.hasAssignmentDateTimetableEntry} ?entry .
          ?entry ${lwm.hasRoom} ?room .
          ?entry ${lwm.hasStartTime} ?startTime .
          ?entry ${lwm.hasEndTime} ?endTime .
          ?entry ${lwm.hasSupervisor} ?supervisor .
          ?group ${lwm.hasLabWork} ?labwork .
          ?labwork ${lwm.hasCourse} ?course .
          ?course ${lwm.hasId} ?courseName .
          ?course ${lwm.hasDegree} ?degree .
          ?degree ${lwm.hasId} ?degreeName .
          ?supervisor ${rdfs.label} ?name .
          ?room ${lwm.hasRoomId} ?roomId .
          ?schedule ${lwm.hasAssignmentDate} "${date.toString("yyyy-MM-dd")}" .
        }
      """.stripMargin
    val result = QueryExecutionFactory.sparqlService(queryHost, query).execSelect()
    var dates = List.empty[(Time, (Resource, String, String, String, String, String, Time, Time, Int))]
    while (result.hasNext) {
      val n = result.nextSolution()
      val groupId = n.getLiteral("groupId").toString
      val startTimeString = URLDecoder.decode(n.getLiteral("startTime").toString, "UTF-8").split(":")
      val startTime = Time(startTimeString(0).toInt, startTimeString(1).toInt)
      val endTimeString = URLDecoder.decode(n.getLiteral("endTime").toString, "UTF-8").split(":")
      val endTime = Time(endTimeString(0).toInt, endTimeString(1).toInt)
      val name = URLDecoder.decode(n.getLiteral("name").toString, "UTF-8")
      val course = URLDecoder.decode(n.getLiteral("courseName").toString, "UTF-8")
      val degree = URLDecoder.decode(n.getLiteral("degreeName").toString, "UTF-8")
      val roomId = URLDecoder.decode(n.getLiteral("roomId").toString, "UTF-8")
      val orderId = if (n.contains("orderId")) URLDecoder.decode(n.getLiteral("orderId").toString, "UTF-8").toInt + 1 else 0
      val groupResource = Resource(n.getResource("group").toString)
      dates = (startTime, (groupResource, course, degree, groupId, roomId, name, startTime, endTime, orderId)) :: dates
    }
    println(dates.size)
    dates.sortBy(_._1)
  }
}

/**
  * An assignment group.
  * @param id id of this group
  * @param labwork id of this associated labwork
  */
case class LabWorkGroup(id: String, labwork: Resource)

object LabworkGroups {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(group: LabWorkGroup): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, rdf.typ, lwm.Group),
      Statement(resource, rdf.typ, owl.NamedIndividual),
      Statement(resource, lwm.hasGroupId, StringLiteral(group.id)),
      Statement(resource, rdfs.label, StringLiteral(group.id)),
      Statement(resource, lwm.hasLabWork, group.labwork),
      Statement(group.labwork, lwm.hasGroup, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(group: LabWorkGroup): Future[LabWorkGroup] = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.Group, Vocabulary.lwm.hasId, StringLiteral(group.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeGroup)
    val p = Promise[LabWorkGroup]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(g ⇒ g.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(group) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.Group)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Group)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }

  def isLabWorkGroup(resource: Resource): Future[Boolean] = sparqlExecutionContext.executeBooleanQuery(s"ASK {$resource ${Vocabulary.rdf.typ} ${lwm.Group}}")
}
