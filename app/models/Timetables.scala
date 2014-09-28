package models

import java.util.UUID

import org.joda.time.{ LocalDate, DateTime }
import play.api.data.Form
import play.api.data.Forms._
import utils.semantic.Vocabulary.LWM
import utils.semantic._

import scala.concurrent.{ Future, Promise }

object Weekdays {

  sealed trait Weekday {
    val index: Int
    val uri: Resource
    val label: String
  }

  case object Monday extends Weekday {
    override val index = 0
    override val uri: Resource = LWM.Monday
    override val label: String = "Montag"
  }

  case object Tuesday extends Weekday {
    override val index = 1
    override val uri: Resource = LWM.Tuesday
    override val label: String = "Dienstag"
  }

  case object Wednesday extends Weekday {
    override val index = 2
    override val uri: Resource = LWM.Wednesday
    override val label: String = "Mittwoch"
  }

  case object Thursday extends Weekday {
    override val index = 3
    override val uri: Resource = LWM.Thursday
    override val label: String = "Donnerstag"
  }

  case object Friday extends Weekday {
    override val index = 4
    override val uri: Resource = LWM.Friday
    override val label: String = "Freitag"
  }

  case object Saturday extends Weekday {
    override val index = 5
    override val uri: Resource = LWM.Saturday
    override val label: String = "Samstag"
  }

  case object Sunday extends Weekday {
    override val index = 6
    override val uri: Resource = LWM.Sunday
    override val label: String = "Sonntag"
  }

  val workWeek = Map(LWM.Monday -> Monday, LWM.Tuesday -> Tuesday, LWM.Wednesday -> Wednesday, LWM.Thursday -> Thursday, LWM.Friday -> Friday, LWM.Saturday -> Saturday)
}

case class Time(hours: Int, minutes: Int) extends Ordered[Time] {
  private val minutesOfDay = hours * 60 + minutes

  override def compare(other: Time): Int = math.signum(minutesOfDay - other.minutesOfDay)

  override def toString = {
    val m = if (minutes < 10) s"0$minutes" else s"$minutes"
    s"$hours:$m"
  }
}

object TimeSlots {
  val slotTimeMap = Map(
    1 -> Time(7, 0),
    2 -> Time(7, 15),
    3 -> Time(7, 30),
    4 -> Time(7, 45),
    5 -> Time(8, 0),
    6 -> Time(8, 15),
    7 -> Time(8, 30),
    8 -> Time(8, 45),
    9 -> Time(9, 0),
    10 -> Time(9, 15),
    11 -> Time(9, 30),
    12 -> Time(9, 45),
    13 -> Time(10, 0),
    14 -> Time(10, 15),
    15 -> Time(10, 30),
    16 -> Time(10, 45),
    17 -> Time(11, 0),
    18 -> Time(11, 15),
    19 -> Time(11, 30),
    20 -> Time(11, 45),
    21 -> Time(12, 0),
    22 -> Time(12, 15),
    23 -> Time(12, 30),
    24 -> Time(12, 45),
    25 -> Time(13, 0),
    26 -> Time(13, 15),
    27 -> Time(13, 30),
    28 -> Time(13, 45),
    29 -> Time(14, 0),
    30 -> Time(14, 15),
    31 -> Time(14, 30),
    32 -> Time(14, 45),
    33 -> Time(15, 0),
    34 -> Time(15, 15),
    35 -> Time(15, 30),
    36 -> Time(15, 45),
    37 -> Time(16, 0),
    38 -> Time(16, 15),
    39 -> Time(16, 30),
    40 -> Time(16, 45),
    41 -> Time(17, 0),
    42 -> Time(17, 15),
    43 -> Time(17, 30),
    44 -> Time(17, 45),
    45 -> Time(18, 0)
  )
}

case class Timetable(labwork: Resource, startDate: LocalDate, endDate: LocalDate, id: UUID = UUID.randomUUID())

case class TimetableEntry(day: Weekdays.Weekday, startTime: Time, endTime: Time, room: Resource, supervisors: List[Resource], timetable: Resource, id: UUID = UUID.randomUUID(), ownResource: Option[Resource] = None)

case class TimetableEntryFormEntry(day: String, startTime: String, endTime: String, room: String, supervisors: String)

object Timetables {

  import utils.Global._
  import utils.semantic.Vocabulary._

  def create(timetable: Timetable) = {
    val resource = ResourceUtils.createResource(lwmNamespace, timetable.id)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Timetable),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasStartDate, DateLiteral(timetable.startDate)),
      Statement(resource, LWM.hasEndDate, DateLiteral(timetable.endDate)),
      Statement(resource, LWM.hasId, StringLiteral(timetable.id.toString)),
      Statement(resource, LWM.hasLabWork, timetable.labwork),
      Statement(timetable.labwork, LWM.hasTimetable, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*))
    Individual(resource)
  }

  def get(resource: Resource): Option[Timetable] = {
    val i = Individual(resource)
    val maybeStartDate = i.props.get(LWM.hasStartDate)
    val maybeEndDate = i.props.get(LWM.hasEndDate)
    val maybeLabwork = i.props.get(LWM.hasLabWork)
    val maybeId = i.props.get(LWM.hasId)
    for {
      startDateList ← maybeStartDate
      endDateList ← maybeEndDate
      labWorkList ← maybeLabwork
      idList ← maybeId
    } yield {
      val labwork = labWorkList.head.asResource().get
      val startDate = LocalDate.parse(startDateList.head.asLiteral().get.decodedString)
      val endDate = LocalDate.parse(endDateList.head.asLiteral().get.decodedString)
      val id = UUID.fromString(idList.head.asLiteral().get.decodedString)
      Timetable(labwork, startDate, endDate, id)
    }
  }

}

object TimetableEntries {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(timetableEntry: TimetableEntry): Future[Individual] = {
    val timetableEntryResource = ResourceUtils.createResource(lwmNamespace, timetableEntry.id)
    val statements = List(
      Statement(timetableEntryResource, RDF.typ, LWM.TimetableEntry),
      Statement(timetableEntryResource, RDF.typ, OWL.NamedIndividual),
      Statement(timetableEntryResource, LWM.hasStartTime, StringLiteral(timetableEntry.startTime.toString)),
      Statement(timetableEntryResource, LWM.hasId, StringLiteral(timetableEntry.id.toString)),
      Statement(timetableEntryResource, LWM.hasEndTime, StringLiteral(timetableEntry.endTime.toString)),
      Statement(timetableEntryResource, LWM.hasTimetable, timetableEntry.timetable),
      Statement(timetableEntry.timetable, LWM.hasTimetableEntry, timetableEntryResource),
      Statement(timetableEntryResource, LWM.hasWeekday, timetableEntry.day.uri),
      Statement(timetableEntryResource, LWM.hasRoom, timetableEntry.room) // TODO das ist wahrscheinlich was falsch
    ) ::: timetableEntry.supervisors.map(supervisor ⇒ List(Statement(timetableEntryResource, LWM.hasSupervisor, supervisor), Statement(supervisor, LWM.isSupervisorFor, timetableEntryResource))).flatten
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(timetableEntryResource)
    }
  }

  def delete(entry: TimetableEntry): Future[TimetableEntry] = {
    val maybeEntry = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.TimetableEntry, Vocabulary.LWM.hasId, StringLiteral(entry.id.toString))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeEntry)
    val p = Promise[TimetableEntry]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(u ⇒ u.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(entry) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.TimetableEntry)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a TimetableEntry"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.TimetableEntry)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(user ⇒ Individual(user.s)).toList
    }
  }

  def get(resource: Resource): Option[TimetableEntry] = {
    val i = Individual(resource)
    val maybeDay = i.props.get(LWM.hasWeekday)
    val maybeRoom = i.props.get(LWM.hasRoom)
    val maybeStartTime = i.props.get(LWM.hasStartTime)
    val maybeEndTime = i.props.get(LWM.hasEndTime)
    val maybeId = i.props.get(LWM.hasId)
    val maybeSupervisors = i.props.get(LWM.hasSupervisor)
    val maybeTimetable = i.props.get(LWM.hasTimetable)
    for {
      dayList ← maybeDay
      roomList ← maybeRoom
      startTimeList ← maybeStartTime
      endTimeList ← maybeEndTime
      idList ← maybeId
      supervisorsList ← maybeSupervisors
      timetableList ← maybeTimetable
    } yield {
      val day = Weekdays.workWeek.get(dayList.head.asResource().get)
      val room = roomList.head.asResource().get
      val startTime = Time(startTimeList.head.asLiteral().get.decodedString.split(":").head.toInt, startTimeList.head.asLiteral().get.decodedString.split(":").last.toInt)
      val endTime = Time(endTimeList.head.asLiteral().get.decodedString.split(":").head.toInt, endTimeList.head.asLiteral().get.decodedString.split(":").last.toInt)
      val id = idList.head.asLiteral().get.decodedString
      val supervisors = supervisorsList.map(_.asResource().get)
      val timetable = timetableList.head.asResource().get
      TimetableEntry(day.get, startTime, endTime, room, supervisors, timetable, UUID.fromString(id), Some(resource))
    }
  }
}

object TimeTableForm {
  val timetableForm = Form(
    mapping(
      "day" -> nonEmptyText,
      "start" -> nonEmptyText,
      "end" -> nonEmptyText,
      "room" -> nonEmptyText,
      "supervisors" -> nonEmptyText
    )(TimetableEntryFormEntry.apply)(TimetableEntryFormEntry.unapply))
}
