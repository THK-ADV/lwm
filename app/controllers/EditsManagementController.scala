package controllers

import java.util.NoSuchElementException
import actors.TransactionsLoggerActor.Transaction
import com.google.common.primitives.Doubles
import models._
import org.joda.time.{ LocalDateTime, LocalDate }
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.{ QueryHost, TransactionSupport, UpdateHost }
import utils.Implicits._
import utils.semantic.Vocabulary._
import utils.semantic._
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import Synchronize._

/**
  * Syntax: Operations must be written in capital letters (for now)
  * CREATE <object> HAVING <properties> WITH <rdfnodes>
  * DELETE <object> HAVING <resource>
  * INSERT <properties> WITH <rdfnodes> IN <resource>
  * REMOVE <properties> WITH <rdfnodes> IN <resource>
  * UPDATE <properties> WITH <rdfnodes> IN <resource>
  *
  * !- Statements, values and resources can directly be added through @{LWM.bla} -!
  * Some examples being:
  * CREATE assignemntAssociation HAVING @{LWM.hasOrderId} WITH @{4}
  * or
  * INSERT @{LWM.hasSemester} WITH @{semester.uri.value} IN @{labwork.uri.value}
  * or
  * DELETE group HAVING @{group.uri.value}
  */
object EditsManagementController extends Controller with Authentication with TransactionSupport {

  import Play.current

  val system = Akka.system

  case class Component(operation: String, mappedValues: mutable.Map[Property, RDFNode], identifier: String)

  def applyEdit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒

        val maybeQuery = (request.body \ "query").asOpt[String]
        val maybeUri = (request.body \ "redirect").asOpt[String]

        if (maybeQuery.isDefined && maybeUri.isDefined) {
          val component = parseQuery(maybeQuery.get)
          component.operation match {
            case "INSERT" ⇒ for (inserted ← insert(component, session.user)) yield if (inserted) Ok(maybeUri.get)
            case "REMOVE" ⇒ for (removed ← remove(component, session.user)) yield if (removed) Ok(maybeUri.get)
            case "UPDATE" ⇒ for (updated ← update(component, session.user)) yield if (updated) Ok(maybeUri.get)
            case "CREATE" ⇒ for (created ← create(component, session.user)(utils.Global.update, utils.Global.query)) yield if (created) Ok(maybeUri.get)
            case "DELETE" ⇒ for (deleted ← delete(component, session.user)(utils.Global.update, utils.Global.query)) yield if (deleted) Ok(maybeUri.get)
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

  private def create(c: Component, user: String)(implicit updateHost: UpdateHost, queryHost: QueryHost): Future[Boolean] = {

    c.identifier match {

      case "group" ⇒

        def groupIdFuture(c: Component) = {
          import utils.Global._

          val query = s"""
          |select (${c.mappedValues.get(lwm.hasLabWork).get} as ?s) (${lwm.hasGroupId} as ?p) ?o where {
          | ${c.mappedValues.get(lwm.hasLabWork).get} ${lwm.hasGroup} ?group .
          | ?group ${lwm.hasGroupId} ?o
          |}
          """.stripMargin

          sparqlExecutionContext.executeQuery(query).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.o)

          }
        }

        val labworkGroupFuture = for {
          groupIds ← groupIdFuture(c)
          labwork = c.mappedValues.get(lwm.hasLabWork).get.asResource().get
        } yield {
          if (groupIds.nonEmpty) LabWorkGroup((groupIds(groupIds.size - 1).value.charAt(0) + 1).asInstanceOf[Char].toString, labwork)
          else LabWorkGroup('A'.toString, labwork)
        }
        for {
          lwk ← labworkGroupFuture
          c ← LabworkGroups.create(lwk)
        } yield {
          createTransaction(user, c.uri, s"Labwork Group $lwk created by $user.")
          true
        }

      case "assignmentassociation" ⇒
        val labwork = c.mappedValues.get(lwm.hasLabWork).get.asResource().get
        val orderId = c.mappedValues.get(lwm.hasOrderId).get.value.toInt
        AssignmentAssociations.create(AssignmentAssociation(labwork, orderId)).map { assignmentAssociation ⇒
          createTransaction(user, assignmentAssociation.uri, s"Labwork Assignment Association for $labwork created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "room" ⇒
        val rId = c.mappedValues.get(lwm.hasRoomId).get
        val name = c.mappedValues.get(lwm.hasName).get
        Rooms.create(Room(rId.value, name.value)).map { room ⇒
          createTransaction(user, room, s"Room ${name.value} created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "course" ⇒
        val name = c.mappedValues.get(lwm.hasName).get
        val id = c.mappedValues.get(lwm.hasId).get
        val degree = c.mappedValues.get(lwm.hasDegree).get.asResource().get
        Courses.create(Course(name.value, id.value, degree)).map { c ⇒
          createTransaction(user, c.uri, s"Course ${name.value} created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "degree" ⇒
        val name = c.mappedValues.get(lwm.hasName).get
        val id = c.mappedValues.get(lwm.hasId).get
        Degrees.create(Degree(name.value, id.value)).map { d ⇒
          createTransaction(user, d.uri, s"Degree ${name.value} created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "labwork" ⇒
        val course = c.mappedValues.get(lwm.hasCourse).get.asResource().get
        val semester = c.mappedValues.get(lwm.hasSemester).get.asResource().get
        LabWorks.create(LabWork(course, semester)).map { l ⇒
          createTransaction(user, l.uri, s"Labwork for course $course created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "semester" ⇒
        val semester: Semester = c.mappedValues.get(lwm.hasId).get.toString.toLowerCase match {
          case "sommersemester" ⇒ SummerSemester(c.mappedValues.get(lwm.hasYear).get.value.toInt)
          case _                ⇒ WinterSemester(c.mappedValues.get(lwm.hasYear).get.value.toInt)
        }
        Semesters.create(semester).map { s ⇒
          createTransaction(user, s.uri, s"$semester created by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "timetable" ⇒
        val labwork = c.mappedValues.get(lwm.hasLabWork).get.asResource().get
        val t = Timetables.create(Timetable(labwork))
        createTransaction(user, t.uri, s"Timetable for $labwork created by $user.")
        Future.successful(true)
    }
  }

  private def delete(c: Component, user: String)(implicit updateHost: UpdateHost, queryHost: QueryHost): Future[Boolean] = {
    val toBeDeleted = c.mappedValues.get(deletion).get.asResource().get
    c.identifier match {
      case "group" ⇒
        LabworkGroups.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"Group $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "assignmentassociation" ⇒
        AssignmentAssociations.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }

      case "room" ⇒
        Rooms.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "course" ⇒
        Courses.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "degree" ⇒
        Degrees.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "labwork" ⇒
        LabWorks.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
      case "semester" ⇒
        Semesters.delete(toBeDeleted).map { l ⇒
          deleteTransaction(user, l, s"AssignmentAssociation $l deleted by $user.")
          true
        }.recover { case NonFatal(t) ⇒ true }
    }
  }

  private def insert(c: Component, user: String): Future[Boolean] = {
    import utils.Global._
    val i = Individual(Resource(c.identifier))
    c.mappedValues.foreach {
      v ⇒ i.add(v._1, v._2)
    }
    Future.successful(true)
  }

  private def remove(c: Component, user: String): Future[Boolean] = {
    import utils.Global._
    val i = Individual(Resource(c.identifier))
    c.mappedValues.foreach { v ⇒
      modifyTransaction(user, i.uri, s"Statement (${v._1}, ${v._2}} removed from ${i.uri} by $user.)")
      i.remove(v._1, v._2)
    }
    Future.successful(true)
  }

  private def update(c: Component, user: String): Future[Boolean] = {
    import utils.Global._
    val i = Individual(Resource(c.identifier))
    val oldValueMap = mutable.Map[Property, RDFNode]()
    c.mappedValues.foreach { v ⇒

      i.props.get(v._1).map { list ⇒
        for (all ← list) yield oldValueMap += v._1 -> all

        oldValueMap.foreach { o ⇒
          i.update(o._1, o._2, c.mappedValues.get(o._1).get)
        }
      }
    }
    Future.successful(true)
  }
}

case object Synchronize {
  final val deletion = lwm.property("removeEntry")

  //IF STATEMENT MISSING, ADD IT TO PATTERN
  def sync(m: Map[String, Any]): mutable.Map[Property, RDFNode] = {
    val newMap = collection.mutable.Map[Property, RDFNode]()
    m.map { e ⇒
      val pattern = e._1.substring(1, e._1.size - 1)
      pattern match {
        case lwm.hasCourse.value                       ⇒ newMap += lwm.hasCourse -> Resource(e._2.toString)
        case lwm.hasLabWork.value                      ⇒ newMap += lwm.hasLabWork -> Resource(e._2.toString)
        case lwm.hasGroup.value                        ⇒ newMap += lwm.hasGroup -> Resource(e._2.toString)
        case lwm.hasOrderId.value                      ⇒ newMap += lwm.hasOrderId -> StringLiteral(e._2.toString)
        case lwm.hasGmId.value                         ⇒ newMap += lwm.hasGmId -> StringLiteral(e._2.toString)
        case lwm.hasEnrollment.value                   ⇒ newMap += lwm.hasEnrollment -> Resource(e._2.toString)
        case lwm.hasApplicant.value                    ⇒ newMap += lwm.hasApplicant -> Resource(e._2.toString)
        case lwm.hasRegistrationId.value               ⇒ newMap += lwm.hasRegistrationId -> StringLiteral(e._2.toString)
        case lwm.hasApplication.value                  ⇒ newMap += lwm.hasApplication -> Resource(e._2.toString)
        case lwm.hasApplicationList.value              ⇒ newMap += lwm.hasApplicationList -> Resource(e._2.toString)
        case lwm.hasAssignment.value                   ⇒ newMap += lwm.hasAssignment -> Resource(e._2.toString)
        case lwm.hasAssignmentAssociation.value        ⇒ newMap += lwm.hasAssignmentAssociation -> Resource(e._2.toString)
        case lwm.hasAssignmentDate.value               ⇒ newMap += lwm.hasAssignmentDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case lwm.hasAssignmentDateTimetableEntry.value ⇒ newMap += lwm.hasAssignmentDateTimetableEntry -> Resource(e._2.toString)
        case lwm.hasBlacklist.value                    ⇒ newMap += lwm.hasBlacklist -> Resource(e._2.toString)
        case lwm.hasBlacklistDate.value                ⇒ newMap += lwm.hasBlacklistDate -> Resource(e._2.toString)
        case lwm.hasDate.value                         ⇒ newMap += lwm.hasDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case lwm.hasDegree.value                       ⇒ newMap += lwm.hasDegree -> Resource(e._2.toString)
        case lwm.hasDescription.value                  ⇒ newMap += lwm.hasDescription -> StringLiteral(e._2.toString)
        case lwm.hasDueDate.value                      ⇒ newMap += lwm.hasDueDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case lwm.hasDueDateTimetableEntry.value        ⇒ newMap += lwm.hasDueDateTimetableEntry -> Resource(e._2.toString)
        case lwm.hasEndDate.value                      ⇒ newMap += lwm.hasEndDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case lwm.hasEndTime.value                      ⇒ newMap += lwm.hasEndTime -> StringLiteral(e._2.toString)
        case lwm.hasFileName.value                     ⇒ newMap += lwm.hasFileName -> StringLiteral(e._2.toString)
        case lwm.hasGroupId.value                      ⇒ newMap += lwm.hasGroupId -> StringLiteral(e._2.toString)
        case lwm.hasId.value                           ⇒ newMap += lwm.hasId -> StringLiteral(e._2.toString)
        case lwm.hasMember.value                       ⇒ newMap += lwm.hasMember -> Resource(e._2.toString)
        case lwm.hasName.value                         ⇒ newMap += lwm.hasName -> StringLiteral(e._2.toString)
        case lwm.hasPartner.value                      ⇒ newMap += lwm.hasPartner -> Resource(e._2.toString)
        case lwm.hasPendingApplication.value           ⇒ newMap += lwm.hasPendingApplication -> Resource(e._2.toString)
        case lwm.hasPreparationTime.value              ⇒ newMap += lwm.hasPreparationTime -> StringLiteral(e._2.toString)
        case lwm.hasRoom.value                         ⇒ newMap += lwm.hasRoom -> Resource(e._2.toString)
        case lwm.hasRoomId.value                       ⇒ newMap += lwm.hasRoomId -> StringLiteral(e._2.toString)
        case lwm.hasScheduleAssociation.value          ⇒ newMap += lwm.hasScheduleAssociation -> Resource(e._2.toString)
        case lwm.hasSemester.value                     ⇒ newMap += lwm.hasSemester -> Resource(e._2.toString)
        case lwm.hasSolution.value                     ⇒ newMap += lwm.hasSolution -> Resource(e._2.toString)
        case lwm.hasStartDate.value                    ⇒ newMap += lwm.hasStartDate -> DateLiteral(LocalDate.parse(e._2.toString))
        case lwm.hasStartTime.value                    ⇒ newMap += lwm.hasStartTime -> StringLiteral(e._2.toString)
        case lwm.hasSupervisor.value                   ⇒ newMap += lwm.hasSupervisor -> Resource(e._2.toString)
        case lwm.hasText.value                         ⇒ newMap += lwm.hasText -> StringLiteral(e._2.toString)
        case lwm.hasTimetable.value                    ⇒ newMap += lwm.hasTimetable -> Resource(e._2.toString)
        case lwm.hasTimetableEntry.value               ⇒ newMap += lwm.hasTimetableEntry -> Resource(e._2.toString)
        case lwm.hasTopic.value                        ⇒ newMap += lwm.hasTopic -> StringLiteral(e._2.toString)
        case lwm.hasWeekday.value                      ⇒ newMap += lwm.hasWeekday -> Resource(e._2.toString)
        case lwm.hasYear.value                         ⇒ newMap += lwm.hasYear -> StringLiteral(e._2.toString)
        case foaf.firstName.value                      ⇒ newMap += foaf.firstName -> StringLiteral(e._2.toString)
        case foaf.lastName.value                       ⇒ newMap += foaf.lastName -> StringLiteral(e._2.toString)
        case rdfs.label.value                          ⇒ newMap += rdfs.label -> StringLiteral(e._2.toString)
        case deletion.value                            ⇒ newMap += deletion -> Resource(e._2.toString)
        case _: String                                 ⇒ println("False match")
      }
    }
    newMap
  }
}
