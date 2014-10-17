package models

import java.net.URLDecoder
import java.util.Date

import com.hp.hpl.jena.query.QueryExecutionFactory
import org.joda.time.{ LocalDate, DateTime }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class LabWork(course: Resource, semester: Resource)

case class LabWorkFormModel(courseId: String, semester: String)

case class LabworkUpdateModel(courseId: String, semester: String, startDate: Date, endDate: Date)

object LabworkExportModes {
  val PublicSchedule = "publicSchedule"
  val PublicGroupMembersTable = "publicMembers"
  val InternalSchedule = "internalSchedule"
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
    val startDate = semesterIndividual.props.getOrElse(LWM.hasStartDate, List(new DateLiteral(LocalDate.now()))).head
    val endDate = semesterIndividual.props.getOrElse(LWM.hasEndDate, List(new DateLiteral(LocalDate.now()))).head

    val courseIndividual = Individual(labWork.course)

    val resource = ResourceUtils.createResource(lwmNamespace)
    val timetable = Timetables.create(Timetable(resource))
    val labworkApplicationList = LabworkApplicationLists.create(LabworkApplicationList(resource))

    val label = courseIndividual.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.asLiteral().get

    val statements = List(
      Statement(resource, RDF.typ, LWM.LabWork),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, RDFS.label, label),
      Statement(resource, LWM.hasTimetable, timetable.uri),
      Statement(resource, LWM.hasCourse, labWork.course),
      Statement(resource, LWM.hasStartDate, startDate),
      Statement(resource, LWM.hasEndDate, endDate),
      Statement(resource, LWM.allowsApplications, StringLiteral("false")),
      Statement(resource, LWM.isClosed, StringLiteral("false")),
      Statement(resource, LWM.hasSemester, labWork.semester)
    )

    labworkApplicationList.flatMap { list ⇒
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
        Individual(resource)
      }
    }
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.LabWork)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LabWork)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }

  def orderedGroups(labwork: Resource) = {
    val query =
      s"""
         |select * where {
         |$labwork ${Vocabulary.LWM.hasGroup} ?group .
         |?group ${Vocabulary.LWM.hasGroupId} ?id .
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
    val query =
      s"""
        select ?course ?groupId ?startTime ?endTime ?name ?courseName ?roomId ?degreeName where {
          ?group ${RDF.typ} ${LWM.Group} .
          ?group ${LWM.hasGroupId} ?groupId .
          ?group ${LWM.hasScheduleAssociation} ?schedule .
          ?schedule ${LWM.hasAssignmentDateTimetableEntry} ?entry .
          ?entry ${LWM.hasRoom} ?room .
          ?entry ${LWM.hasStartTime} ?startTime .
          ?entry ${LWM.hasEndTime} ?endTime .
          ?entry ${LWM.hasSupervisor} ?supervisor .
          ?group ${LWM.hasLabWork} ?labwork .
          ?labwork ${LWM.hasCourse} ?course .
          ?course ${LWM.hasId} ?courseName .
          ?course ${LWM.hasDegree} ?degree .
          ?degree ${LWM.hasId} ?degreeName .
          ?supervisor ${RDFS.label} ?name .
          ?room ${LWM.hasRoomId} ?roomId .
          ?schedule ${LWM.hasAssignmentDate} "${date.toString("yyyy-MM-dd")}" .
        }
      """.stripMargin
    val result = QueryExecutionFactory.sparqlService(queryHost, query).execSelect()
    var dates = List.empty[(Time, (Resource, String, String, String, String, String, Time, Time))]
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
      val courseResource = Resource(n.getResource("course").toString)
      dates = (startTime, (courseResource, course, degree, groupId, roomId, name, startTime, endTime)) :: dates
    }
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
      Statement(resource, RDF.typ, LWM.Group),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasGroupId, StringLiteral(group.id)),
      Statement(resource, RDFS.label, StringLiteral(group.id)),
      Statement(resource, LWM.hasLabWork, group.labwork),
      Statement(group.labwork, LWM.hasGroup, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(group: LabWorkGroup): Future[LabWorkGroup] = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Group, Vocabulary.LWM.hasId, StringLiteral(group.id))
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
    if (individual.props(RDF.typ).contains(LWM.Group)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Group)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }

  def isLabWorkGroup(resource: Resource): Future[Boolean] = sparqlExecutionContext.executeBooleanQuery(s"ASK {$resource ${Vocabulary.RDF.typ} ${LWM.Group}}")
}
