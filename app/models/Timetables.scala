package models

import play.api.data.Form
import play.api.data.Forms._
import utils.semantic._
import utils.semantic.Vocabulary.LWM

import scala.concurrent.Future


object Weekdays{
  sealed trait Weekday{
    val uri: Resource
    val label: String
  }
  case object Monday extends Weekday {
    override val uri: Resource = LWM.Monday
    override val label: String = "Montag"
  }
  case object Tuesday extends Weekday {
    override val uri: Resource = LWM.Tuesday
    override val label: String = "Dienstag"
  }
  case object Wednesday extends Weekday {
    override val uri: Resource = LWM.Wednesday
    override val label: String = "Mittwoch"
  }
  case object Thursday extends Weekday {
    override val uri: Resource = LWM.Thursday
    override val label: String = "Donnerstag"
  }
  case object Friday extends Weekday {
    override val uri: Resource = LWM.Friday
    override val label: String = "Freitag"
  }
  case object Saturday extends Weekday {
    override val uri: Resource = LWM.Saturday
    override val label: String = "Samstag"
  }
  case object Sunday extends Weekday {
    override val uri: Resource = LWM.Sunday
    override val label: String = "Sonntag"
  }

  val workWeek = List(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
}

case class Time(hours: Int, minutes: Int) extends Ordered[Time]{
  private val minutesOfDay = hours * 60 + minutes

  override def compare(other: Time): Int = math.signum(minutesOfDay - other.minutesOfDay)
  override def toString = s"$hours:$minutes"
}

object TimeSlots{
  val slotTimeMap = Map(
    1 -> Time(7,30),
    2 -> Time(8,15),
    3 -> Time(9,0),
    4 -> Time(9,45),
    5 -> Time(10,30),
    6 -> Time(11,15),
    7 -> Time(12,0),
    8 -> Time(12,45),
    9 -> Time(13,30),
    10 -> Time(14,15),
    11 -> Time(15,0),
    12 -> Time(15,45)
  )
}

case class Timetable(labwork: Resource)

case class TimetableEntry(day: Weekdays.Weekday, startTime: Time, endTime: Time, room: String, supervisors: List[Resource], timetable: Resource)
case class TimetableEntryFormEntry(day: String, startTime: String, endTime: String, room: String, supervisors: String)


object TimetableEntries{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(timetableSlot: TimetableEntry) = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.TimetableEntry),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasStartTime, Literal(timetableSlot.startTime.toString)),
      Statement(resource, LWM.hasEndTime, Literal(timetableSlot.endTime.toString)),
      Statement(resource, LWM.hasTimetable, timetableSlot.timetable),
      Statement(resource, LWM.hasWeekday, timetableSlot.day.uri)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def entriesForTimetable(timetable: Resource) = ???

  def all(): Future[Seq[Individual]] = Future{
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.TimetableEntry))).map(timetableEntry => Individual(timetableEntry.s))
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
