package models

import utils.semantic._
import utils.semantic.Vocabulary.LWM


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

case class TimetableSlot(day: Weekdays.Weekday, startTime: Time, endTime: Time, possibleTeachers: List[Individual], timetable: Individual)

case class Schedule()


object Schedules {

}

object TimetableSlots{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(timetableSlot: TimetableSlot) = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.TimetableSlot),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasStartTime, Literal(timetableSlot.startTime.toString)),
      Statement(resource, LWM.hasEndTime, Literal(timetableSlot.endTime.toString)),
      Statement(resource, LWM.hasTimetable, timetableSlot.timetable.uri),
      Statement(resource, LWM.hasWeekday, timetableSlot.day.uri)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }
}
