package controllers

import java.util.NoSuchElementException

import models._
import org.joda.time.LocalDate
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ OWL, FOAF, RDF, LWM }
import utils.semantic._
import utils.Global._
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import Synchronize._

/**
  * Syntax: Operations must be written in capital letters (for now)
  * CREATE <object> HAVING <statements> WITH <values>
  * DELETE <object> HAVING <resource>
  * INSERT <statements> WITH <values> IN <resource>
  * REMOVE <statements> WITH <values> IN <resource>
  * UPDATE <statements> WITH <values> IN <resource>
  *
  * !- Statements, values and resources can directly be added through @{LWM.bla} -!
  * Some examples being:
  * CREATE assignemntAssociation HAVING @{LWM.hasOrderId} WITH @{4}
  * or
  * INSERT @{LWM.hasSemester} WITH @{semester.uri.value} IN @{labwork.uri.value}
  * or
  * DELETE group HAVING @{group.uri.value}
  */
object EditsManagementController extends Controller with Authentication {

  case class Component(operation: String, mappedValues: mutable.Map[Property, RDFNode], identifier: String)

  def applyEdit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒
        val maybeQuery = (request.body \ "query").asOpt[String]
        val maybeUri = (request.body \ "redirect").asOpt[String]

        if (maybeQuery.isDefined && maybeUri.isDefined) {
          val component = parseQuery(maybeQuery.get)

          component.operation match {
            case "INSERT" ⇒ for (inserted ← insert(component)) yield if (inserted) Ok(maybeUri.get)
            case "REMOVE" ⇒ for (removed ← remove(component)) yield if (removed) Ok(maybeUri.get)
            case "UPDATE" ⇒ for (updated ← update(component)) yield if (updated) Ok(maybeUri.get)
            case "CREATE" ⇒ for (created ← create(component)) yield if (created) Ok(maybeUri.get)
            case "DELETE" ⇒ for (deleted ← delete(component)) yield if (deleted) Ok(maybeUri.get)
          }
        }

        Future.successful(Redirect(routes.LabworkManagementController.index()))
      }
  }

  private def parseQuery(q: String): Component = {
    val operation = Map("INSERT" -> 1, "REMOVE" -> 1, "UPDATE" -> 1, "CREATE" -> 2, "DELETE" -> 3).filter(e ⇒ q.contains(e._1))

    operation.headOption.fold(Component("none", mutable.Map(), "")) { o ⇒
      o._2 match {
        case 1 ⇒
          val statements = q.split(s"${o._1}")(1).split("WITH")(0).trim.split(",")
          val values = q.split(s"${o._1}")(1).split("WITH")(1).split("IN")(0).trim.split(",")

          val mappedValues = sync(statements.zipWithIndex.map(p ⇒ (p._1.trim, values(p._2).trim)).toMap)
          val resource = q.split(s"${o._1}")(1).split("WITH")(1).split("IN")(1).trim
          Component(o._1, mappedValues, resource)

        case 2 ⇒
          val t = q.split(s"${o._1}")(1).split("HAVING")(0).toLowerCase.trim
          val s = q.split(s"${o._1}")(1).split("HAVING")(1).split("WITH")(0).trim.split(",")
          val v = q.split(s"${o._1}")(1).split("HAVING")(1).split("WITH")(1).trim.split(",")
          val mappedValues = sync(s.zipWithIndex.map(p ⇒ (p._1.trim, v(p._2).trim)).toMap)
          Component(o._1, mappedValues, t)

        case 3 ⇒
          val t = q.split(s"${o._1}")(1).split("HAVING")(0).trim.toLowerCase
          val r = q.split(s"${o._1}")(1).split("HAVING")(1).trim
          val mappedValues = sync(Map(s"$deletion" -> r))
          Component(o._1, mappedValues, t)
      }
    }
  }

  private def create(c: Component): Future[Boolean] = {
    c.identifier match {
      case "group" ⇒
        val groupQuery =
          s"""
          |select (${c.mappedValues.get(LWM.hasLabWork).get} as ?s) (${LWM.hasGroupId} as ?p) ?o where {
          | ${c.mappedValues.get(LWM.hasLabWork).get} ${LWM.hasGroup} ?group .
          | ?group ${LWM.hasGroupId} ?o
          |}
        """.stripMargin

        val groupIdFuture = sparqlExecutionContext.executeQuery(groupQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val labworkGroupFuture = for {
          groupIds ← groupIdFuture
          labwork = c.mappedValues.get(LWM.hasLabWork).get.asResource().get
        } yield {
          if (groupIds.nonEmpty) LabWorkGroup((groupIds(groupIds.size - 1).value.charAt(0) + 1).asInstanceOf[Char].toString, labwork)
          else LabWorkGroup('A'.toString, labwork)
        }
        for {
          lwk ← labworkGroupFuture
          c ← LabworkGroups.create(lwk)
        } yield {
          true
        }

      case "assignmentassociation" ⇒
        val labwork = c.mappedValues.get(LWM.hasLabWork).get.asResource().get
        val orderId = c.mappedValues.get(LWM.hasOrderId).get.value.toInt
        AssignmentAssociations.create(AssignmentAssociation(labwork, orderId)).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "room" ⇒
        val rId = c.mappedValues.get(LWM.hasRoomId).get
        val name = c.mappedValues.get(LWM.hasName).get
        Rooms.create(Room(rId.value, name.value)).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "course" ⇒
        val name = c.mappedValues.get(LWM.hasName).get
        val id = c.mappedValues.get(LWM.hasId).get
        val degree = c.mappedValues.get(LWM.hasDegree).get.asResource().get
        Courses.create(Course(name.value, id.value, degree)).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "degree" ⇒
        val name = c.mappedValues.get(LWM.hasName).get
        val id = c.mappedValues.get(LWM.hasId).get
        Degrees.create(Degree(name.value, id.value)).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "labwork" ⇒
        val course = c.mappedValues.get(LWM.hasCourse).get.asResource().get
        val semester = c.mappedValues.get(LWM.hasSemester).get.asResource().get
        LabWorks.create(LabWork(course, semester)).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "semester" ⇒
        val semester: Semester = c.mappedValues.get(LWM.hasId).get.toString.toLowerCase match {
          case "sommersemester" ⇒ SummerSemester(c.mappedValues.get(LWM.hasYear).get.value.toInt)
          case _                ⇒ WinterSemester(c.mappedValues.get(LWM.hasYear).get.value.toInt)
        }
        Semesters.create(semester).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "timetable" ⇒
        val labwork = c.mappedValues.get(LWM.hasLabWork).get.asResource().get
        val sD = c.mappedValues.get(LWM.hasStartDate).get.asInstanceOf[LocalDate]
        val eD = c.mappedValues.get(LWM.hasEndDate).get.asInstanceOf[LocalDate]
        Timetables.create(Timetable(labwork, sD, eD))
        Future.successful(true)
    }
  }

  private def delete(c: Component): Future[Boolean] = {
    val toBeDeleted = c.mappedValues.get(deletion).get.asResource().get
    c.identifier match {
      case "group"                 ⇒ LabworkGroups.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "assignmentassociation" ⇒ AssignmentAssociations.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "room"                  ⇒ Rooms.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "course"                ⇒ Courses.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "degree"                ⇒ Degrees.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "labwork"               ⇒ LabWorks.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
      case "semester"              ⇒ Semesters.delete(toBeDeleted).map(_ ⇒ true).recover { case NonFatal(t) ⇒ true }
    }
  }

  private def insert(c: Component): Future[Boolean] = {
    val i = Individual(Resource(c.identifier))
    c.mappedValues.foreach {
      v ⇒ i.add(v._1, v._2)
    }
    Future.successful(true)
  }

  private def remove(c: Component): Future[Boolean] = {
    val i = Individual(Resource(c.identifier))
    c.mappedValues.foreach { v ⇒
      i.remove(v._1, v._2)
    }
    Future.successful(true)
  }

  private def update(c: Component): Future[Boolean] = {
    val i = Individual(Resource(c.identifier))
    val oldValueMap = mutable.Map[Property, RDFNode]()
    c.mappedValues.foreach { v ⇒
      // TODO i.props.get, nicht props und try catch
      try {
        val list = i.props(v._1)
        for (all ← list) yield oldValueMap += v._1 -> all

        oldValueMap.foreach { o ⇒
          i.update(o._1, o._2, c.mappedValues.get(o._1).get)
        }
      } catch {
        case nse: NoSuchElementException ⇒ println("Non existent key")
        case NonFatal(t)                 ⇒ println("Internal error")
      }
    }
    Future.successful(true)
  }
}

case object Synchronize {
  final val deletion = LWM.property("removeEntry")

  //IF STATEMENT MISSING, ADD IT TO PATTERN
  def sync(m: Map[String, Any]): mutable.Map[Property, RDFNode] = {
    val newMap = collection.mutable.Map[Property, RDFNode]()
    m.map { e ⇒
      val pattern = e._1.substring(1, e._1.size - 1)
      pattern match {
        case LWM.hasCourse.value                       ⇒ newMap += LWM.hasCourse -> Resource(e._2.toString)
        case LWM.hasLabWork.value                      ⇒ newMap += LWM.hasLabWork -> Resource(e._2.toString)
        case LWM.hasGroup.value                        ⇒ newMap += LWM.hasGroup -> Resource(e._2.toString)
        case LWM.hasOrderId.value                      ⇒ newMap += LWM.hasOrderId -> StringLiteral(e._2.toString)
        case LWM.hasGmId.value                         ⇒ newMap += LWM.hasGmId -> StringLiteral(e._2.toString)
        case LWM.hasEnrollment.value                   ⇒ newMap += LWM.hasEnrollment -> Resource(e._2.toString)
        case LWM.hasApplicant.value                    ⇒ newMap += LWM.hasApplicant -> Resource(e._2.toString)
        case LWM.hasRegistrationId.value               ⇒ newMap += LWM.hasRegistrationId -> StringLiteral(e._2.toString)
        case LWM.hasApplication.value                  ⇒ newMap += LWM.hasApplication -> Resource(e._2.toString)
        case LWM.hasApplicationList.value              ⇒ newMap += LWM.hasApplicationList -> Resource(e._2.toString)
        case LWM.hasAssignment.value                   ⇒ newMap += LWM.hasAssignment -> Resource(e._2.toString)
        case LWM.hasAssignmentAssociation.value        ⇒ newMap += LWM.hasAssignmentAssociation -> Resource(e._2.toString)
        case LWM.hasAssignmentDate.value               ⇒ newMap += LWM.hasAssignmentDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case LWM.hasAssignmentDateTimetableEntry.value ⇒ newMap += LWM.hasAssignmentDateTimetableEntry -> Resource(e._2.toString)
        case LWM.hasBlacklist.value                    ⇒ newMap += LWM.hasBlacklist -> Resource(e._2.toString)
        case LWM.hasBlacklistDate.value                ⇒ newMap += LWM.hasBlacklistDate -> Resource(e._2.toString)
        case LWM.hasDate.value                         ⇒ newMap += LWM.hasDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case LWM.hasDegree.value                       ⇒ newMap += LWM.hasDegree -> Resource(e._2.toString)
        case LWM.hasDescription.value                  ⇒ newMap += LWM.hasDescription -> StringLiteral(e._2.toString)
        case LWM.hasDueDate.value                      ⇒ newMap += LWM.hasDueDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case LWM.hasDueDateTimetableEntry.value        ⇒ newMap += LWM.hasDueDateTimetableEntry -> Resource(e._2.toString)
        case LWM.hasEndDate.value                      ⇒ newMap += LWM.hasEndDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case LWM.hasEndTime.value                      ⇒ newMap += LWM.hasEndTime -> StringLiteral(e._2.toString)
        case LWM.hasFileName.value                     ⇒ newMap += LWM.hasFileName -> StringLiteral(e._2.toString)
        case LWM.hasGroupId.value                      ⇒ newMap += LWM.hasGroupId -> StringLiteral(e._2.toString)
        case LWM.hasId.value                           ⇒ newMap += LWM.hasId -> StringLiteral(e._2.toString)
        case LWM.hasMember.value                       ⇒ newMap += LWM.hasMember -> Resource(e._2.toString)
        case LWM.hasName.value                         ⇒ newMap += LWM.hasName -> StringLiteral(e._2.toString)
        case LWM.hasPartner.value                      ⇒ newMap += LWM.hasPartner -> Resource(e._2.toString)
        case LWM.hasPendingApplication.value           ⇒ newMap += LWM.hasPendingApplication -> Resource(e._2.toString)
        case LWM.hasPreparationTime.value              ⇒ newMap += LWM.hasPreparationTime -> StringLiteral(e._2.toString)
        case LWM.hasRoom.value                         ⇒ newMap += LWM.hasRoom -> Resource(e._2.toString)
        case LWM.hasRoomId.value                       ⇒ newMap += LWM.hasRoomId -> StringLiteral(e._2.toString)
        case LWM.hasScheduleAssociation.value          ⇒ newMap += LWM.hasScheduleAssociation -> Resource(e._2.toString)
        case LWM.hasSemester.value                     ⇒ newMap += LWM.hasSemester -> Resource(e._2.toString)
        case LWM.hasSolution.value                     ⇒ newMap += LWM.hasSolution -> Resource(e._2.toString)
        case LWM.hasStartDate.value                    ⇒ newMap += LWM.hasStartDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case LWM.hasStartTime.value                    ⇒ newMap += LWM.hasStartTime -> StringLiteral(e._2.toString)
        case LWM.hasSupervisor.value                   ⇒ newMap += LWM.hasSupervisor -> Resource(e._2.toString)
        case LWM.hasText.value                         ⇒ newMap += LWM.hasText -> StringLiteral(e._2.toString)
        case LWM.hasTimetable.value                    ⇒ newMap += LWM.hasTimetable -> Resource(e._2.toString)
        case LWM.hasTimetableEntry.value               ⇒ newMap += LWM.hasTimetableEntry -> Resource(e._2.toString)
        case LWM.hasTopic.value                        ⇒ newMap += LWM.hasTopic -> StringLiteral(e._2.toString)
        case LWM.hasWeekday.value                      ⇒ newMap += LWM.hasWeekday -> Resource(e._2.toString)
        case LWM.hasYear.value                         ⇒ newMap += LWM.hasYear -> StringLiteral(e._2.toString)
        case FOAF.firstName.value                      ⇒ newMap += FOAF.firstName -> StringLiteral(e._2.toString)
        case FOAF.lastName.value                       ⇒ newMap += FOAF.lastName -> StringLiteral(e._2.toString)
        case deletion.value                            ⇒ newMap += deletion -> Resource(e._2.toString)
        case _: String                                 ⇒ println("False match")
      }
    }
    newMap
  }
}
