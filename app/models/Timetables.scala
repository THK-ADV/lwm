package models

import utils.semantic._
import utils.semantic.Vocabulary.LWM

import scala.concurrent.Future


object Weekdays{
  sealed trait Weekday{
    val uri: Resource
  }
  case object Monday extends Weekday {
    override val uri: Resource = LWM.Monday
  }
  case object Tuesday extends Weekday {
    override val uri: Resource = LWM.Tuesday
  }
  case object Wednesday extends Weekday {
    override val uri: Resource = LWM.Wednesday
  }
  case object Thursday extends Weekday {
    override val uri: Resource = LWM.Thursday
  }
  case object Friday extends Weekday {
    override val uri: Resource = LWM.Friday
  }
  case object Saturday extends Weekday {
    override val uri: Resource = LWM.Saturday
  }
  case object Sunday extends Weekday {
    override val uri: Resource = LWM.Sunday
  }
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


object Timetables{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(timetable: Timetable) = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Timetable),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(timetable.labwork, LWM.hasTimetable, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

}


object TimetableEntries{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(timetableEntry: TimetableEntry) = {
    val timetableEntryResource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(timetableEntryResource, RDF.typ, LWM.TimetableEntry),
      Statement(timetableEntryResource, RDF.typ, OWL.NamedIndividual),
      Statement(timetableEntryResource, LWM.hasStartTime, Literal(timetableEntry.startTime.toString)),
      Statement(timetableEntryResource, LWM.hasEndTime, Literal(timetableEntry.endTime.toString)),
      Statement(timetableEntryResource, LWM.hasTimetable, timetableEntry.timetable),
      Statement(timetableEntry.timetable, LWM.hasEntry, timetableEntryResource),
      Statement(timetableEntryResource, LWM.hasWeekday, timetableEntry.day.uri)
    ) ::: timetableEntry.supervisors.map(supervisor => List(Statement(timetableEntryResource, LWM.hasSupervisor, supervisor), Statement(supervisor, LWM.supervises, timetableEntryResource))).flatten
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(timetableEntryResource)
  }


  def all(): Future[Seq[Individual]] = Future{
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.TimetableEntry))).map(timetableEntry => Individual(timetableEntry.s))
  }
}
