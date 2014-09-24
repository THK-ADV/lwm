package utils

import models._
import org.joda.time.LocalDate
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Individual, Resource }

import scala.annotation.tailrec

object SemesterDatesGenerator {
  case class AssignmentDateAssociation(group: (String, Resource), association: (Int, (Resource, Int)), date: LocalDate)
  case class DueDateAssociation(group: Resource, association: Resource, date: LocalDate)

  import utils.Global._

  private def generateDateList(startDate: LocalDate, endDate: LocalDate): List[LocalDate] = {
    @tailrec
    def generateDateList(next: LocalDate, list: List[LocalDate]): List[LocalDate] = {
      if (next.compareTo(endDate) >= 0) next :: list else generateDateList(next.plusWeeks(1), next :: list)
    }

    generateDateList(startDate, Nil)
  }

  def apply(timetable: Resource, semester: Resource) = {
    Timetables.get(timetable).map { t ⇒

      val entryResource = Individual(timetable).props.getOrElse(LWM.hasEntry, Nil)
      val entries = entryResource.map { entry ⇒
        TimetableEntries.get(entry.asResource().get)
      }.flatten

      val available = entries.map { entry ⇒
        val firstDate = t.startDate.plusDays(entry.day.index)
        val lastDate = t.endDate.plusDays(entry.day.index)
        generateDateList(firstDate, lastDate)
      }.flatten

      val holidays = (Holidays(t.startDate.year().get()) ++ Holidays(t.endDate.year().get())).toMap.keys.toList

      var possibleDates = (available diff holidays).sortWith((a, b) ⇒ a.compareTo(b) < 0)

      val labwork = Individual(t.labwork)
      val groupCount = labwork.props.getOrElse(LWM.hasGroup, Nil).size
      val assignmentCount = labwork.props.getOrElse(LWM.hasAssignmentCount, List(StringLiteral("0"))).head.asLiteral().get.value.toInt

      if (possibleDates.size < groupCount * assignmentCount) {
        println(s"ERROR: Not enough available assignment dates for $groupCount groups with $assignmentCount assignments")
        println(possibleDates)
      }

      val orderedAssocs = (for {
        assocNode ← labwork.props.getOrElse(LWM.hasAssignmentAssociation, Nil)
        assoc = assocNode.asResource().get
        orderIdNode ← Individual(assoc).props.getOrElse(LWM.hasOrderId, Nil)
        orderId = orderIdNode.asLiteral().get
        prepTime = Individual(assoc).props.getOrElse(LWM.hasPreparationTime, Nil).headOption.getOrElse(StringLiteral("0")).value.toInt
      } yield orderId.decodedString.toInt -> (assoc, prepTime.toInt)).sortBy(_._1)

      val orderedGroups = (for {
        groupNode ← labwork.props.getOrElse(LWM.hasGroup, Nil)
        group = groupNode.asResource().get
        groupIdNode ← Individual(group).props.getOrElse(LWM.hasGroupId, Nil)
        orderId = groupIdNode.asLiteral().get
      } yield orderId.decodedString -> group).sortBy(_._1)

      val assignmentDates = for {
        assoc ← orderedAssocs
        group ← orderedGroups
      } yield {
        val date = possibleDates.head
        possibleDates = possibleDates.tail
        AssignmentDateAssociation(group, assoc, date)
      }

      val assignmentsPerGroup = assignmentDates.groupBy(_.group._2).map { group ⇒
        group._1 -> group._2.sortWith((a, b) ⇒ a.date.compareTo(b.date) < 0)
      }

      val dues = assignmentsPerGroup.map { entry ⇒
        var dates = entry._2
        var d = List[DueDateAssociation]()
        while (dates.size > 0) {
          val current = dates.head
          val prepTime = current.association._2._2
          val dueDate = dates.drop(prepTime).head
          dates = dates.tail
          d = DueDateAssociation(current.group._2, current.association._2._1, dueDate.date) :: d
        }
        d
      }.flatten.groupBy(e ⇒ (e.association, e.group))

      val schedule = assignmentDates.map { date ⇒
        val due = dues((date.association._2._1, date.group._2)).head
        ScheduleAssociation(date.group._2, date.association._2._1, date.date, due.date)
      }

      schedule.map { s ⇒
        ScheduleAssociations.create(s)
      }

    }
  }
}
