package utils

import java.util.Locale

import de.jollyday.{ HolidayCalendar, HolidayManager }

object Holidays {
  import scala.collection.JavaConverters._
  private val manager = HolidayManager.getInstance(HolidayCalendar.GERMANY)
  val dates = manager.getHolidays(2014, "nw").asScala.map(holiday => holiday.getDate -> holiday.getDescription(Locale.forLanguageTag("de")))
}
