package utils

import java.util.Locale

import de.jollyday.{ HolidayCalendar, HolidayManager }

object Holidays {
  import scala.collection.JavaConverters._
  private val manager = HolidayManager.getInstance(HolidayCalendar.GERMANY)
  def apply(year: Int) = manager.getHolidays(year, "nw").asScala.map(holiday ⇒ holiday.getDate -> holiday.getDescription(Locale.forLanguageTag("de")))

}
