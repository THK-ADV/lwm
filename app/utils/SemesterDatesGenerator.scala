package utils

import java.util.UUID

import models._
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ Individual, Resource }

/**
  * Created by rgiacinto on 22/09/14.
  */
object SemesterDatesGenerator {
  import Global._

  def apply(timetable: Resource, semester: Resource) = {
    Timetables.get(timetable).map { t ⇒
      val entryResource = Individual(timetable).props.getOrElse(LWM.hasEntry, Nil)
      val entries = entryResource.map { entry ⇒
        TimetableEntries.get(entry.asResource().get)
      }.flatten

      entries.map { entry ⇒
        println(entry)
      }
    }
  }
}
