package controllers

import models._
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Result, Call, Action, Controller }
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic._
import utils.semantic.Vocabulary.{ rdfs, rdf, lwm, owl }
import utils.Global._
import utils.Implicits._
import scala.concurrent.{ Promise, Future, blocking }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
object GroupManagementController extends Controller with Authentication with TransactionSupport {
  import play.api.Play.current
  val system = Akka.system

  def index(labworkId: String, groupId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async { implicit request ⇒
        val query =
          s"""
         select ?s (${lwm.hasGroupId} as ?p) ?o where {
          ?s ${rdf.typ} ${lwm.Group} .
          ?s ${lwm.hasLabWork} <$labworkId> .
          ?s ${lwm.hasGroupId} ?o
          }
          ORDER BY ASC(?o)
            """.stripMargin

        val futureGroups = sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(r ⇒ Individual(r.s))
        }

        futureGroups.map { g ⇒
          val lI = Individual(Resource(labworkId))
          val gI = Individual(Resource(groupId))
          val s = gI.props.getOrElse(lwm.hasMember, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          val a = lI.props.getOrElse(lwm.hasAssignmentAssociation, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          Ok(views.html.groups_detail_management(lI, gI, s, g.toList, a))
        }.recover {
          case NonFatal(e) ⇒
            InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
        }
      }
  }

  def studentAddition(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val maybeStudent = (request.body \ "student").asOpt[String]
      val maybeGroup = (request.body \ "group").asOpt[String]

      if (!maybeGroup.isDefined || !maybeStudent.isDefined) {
        Future.successful(Redirect(routes.LabworkManagementController.index()))
      } else {
        val studentResource = Resource(maybeStudent.get)
        val groupResource = Resource(maybeGroup.get)
        for {
          isGroup ← LabworkGroups.isLabWorkGroup(groupResource)
        } yield {
          if (Students.check(studentResource) && isGroup) {
            ScheduleAssociations.getForGroup(groupResource).map { assocs ⇒
              assocs.map { assoc ⇒
                ScheduleAssociations.create(assoc, studentResource).map { i ⇒
                  createTransaction(session.user, i.uri, s"Schedule ${i.uri} created for Student $studentResource by ${session.user}")
                }
              }
            }
            val ig = Individual(groupResource)

            ig.add(lwm.hasMember, studentResource)
            createTransaction(session.user, ig.uri, s"Student $studentResource added to Group ${ig.uri} by ${session.user}")
            val is = Individual(studentResource)
            is.add(lwm.memberOf, groupResource)
            createTransaction(session.user, ig.uri, s"Student $studentResource added to Group ${ig.uri} by ${session.user}")

            val applicationsFuture = findApplication(groupResource, studentResource)

            applicationsFuture.map { applications ⇒
              applications.map { application ⇒
                sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(application))
              }
            }

          }
          Redirect(routes.LabworkManagementController.index())
        }
      }
    }
  }

  def findApplication(group: Resource, student: Resource) = {
    val query =
      s"""
         select ($student as ?s) (${lwm.hasApplication} as ?p) (?application as ?o) where {
          $student ${lwm.hasPendingApplication} ?application .
          ?application ${lwm.hasLabWork} ?labwork .
          ?labwork ${lwm.hasGroup} $group .
        }
       """.stripMargin

    sparqlExecutionContext.executeQuery(query).map { result ⇒
      SPARQLTools.statementsFromString(result).map(_.o.asResource()).flatten
    }
  }

  def studentRemoval(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒
        val maybeStudent = (request.body \ "student").asOpt[String]
        val maybeGroup = (request.body \ "group").asOpt[String]

        if (!maybeGroup.isDefined || !maybeStudent.isDefined) {
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        } else {
          val studentResource = Resource(maybeStudent.get)
          val groupResource = Resource(maybeGroup.get)
          for {
            isGroup ← LabworkGroups.isLabWorkGroup(groupResource)
          } yield {
            if (Students.check(studentResource) && isGroup) {
              val query =
                s"""
                   |select ($studentResource as ?s) (${lwm.hasScheduleAssociation} as ?p) (?ass as ?o) where {
                   | $studentResource ${lwm.hasScheduleAssociation} ?ass .
                   | ?ass ${lwm.hasGroup} $groupResource
                   |}
                 """.stripMargin

              SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
                sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(statement.o.asResource().get))
              }

              val ig = Individual(groupResource)
              ig.remove(lwm.hasMember, studentResource)
              val is = Individual(studentResource)
              is.remove(lwm.memberOf, groupResource)
              deleteTransaction(session.user, groupResource, s"Student $studentResource removed from Group $groupResource by ${session.user}")
            }
            Redirect(routes.LabworkManagementController.index())
          }
        }
      }
  }

  def swapGroups = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val maybeStudent = (request.body \ "student").asOpt[String]
        val maybeOldGroup = (request.body \ "old").asOpt[String]
        val maybeNewGroup = (request.body \ "new").asOpt[String]
        if (maybeStudent.isDefined && maybeOldGroup.isDefined && maybeNewGroup.isDefined) {
          val student = Resource(maybeStudent.get)
          val oldGroup = Resource(maybeOldGroup.get)
          val newGroup = Resource(maybeNewGroup.get)

          val studentSchedule =
            s"""
               |${Vocabulary.defaultPrefixes}
               |
               | Select ?assignment ?schedule where {
               |    $student lwm:hasScheduleAssociation ?schedule .
               |    ?schedule lwm:hasGroup $oldGroup .
               |    ?schedule lwm:hasAssignmentAssociation ?assignment .
               |
               | } order by desc(?assignment)
             """.stripMargin.execSelect().map(qs ⇒ Resource(qs.data("schedule").toString))

          val newGroupSchedule =
            s"""
               |${Vocabulary.defaultPrefixes}
               |
               | Select ?assignment ?schedule where {
               |    $newGroup lwm:hasScheduleAssociation ?schedule .
               |    ?schedule lwm:hasAssignmentAssociation ?assignment .
               |
               | } order by desc(?assignment)
             """.stripMargin.execSelect().map(qs ⇒ Resource(qs.data("schedule").toString))

          def buildDelete() = {
            val builder = StringBuilder.newBuilder
            builder.append(Vocabulary.defaultPrefixes)
            s"""
               |${Vocabulary.defaultPrefixes}
               |
               | Select * where {
               |    $student lwm:hasScheduleAssociation ?schedule .
               |    ?schedule lwm:hasGroup $oldGroup .
               |    ?schedule lwm:hasDueDateTimetableEntry ?dueDateEntry .
               |    ?schedule lwm:hasDueDate ?dueDate .
               |    ?schedule lwm:hasAssignmentDateTimetableEntry ?assignmentEntry .
               |    ?schedule lwm:hasAssignmentDate ?assignmentDate .
               |    ?schedule lwm:hasAssignmentAssociation ?assignment .
               | }
             """.stripMargin.execSelect().map { qs ⇒

              val schedule = Resource(qs.data("schedule").toString)
              val dueDateEntry = Resource(qs.data("dueDateEntry").toString)
              val dueDate = s"'${qs.data("dueDate").toString}'"
              val assignmentEntry = Resource(qs.data("assignmentEntry").toString)
              val assignmentDate = s"'${qs.data("assignmentDate").toString}'"
              val assignment = Resource(qs.data("assignment").toString)

              builder.append(
                s"""
                     |Delete data {
                     |  $schedule lwm:hasDueDateTimetableEntry $dueDateEntry .
                     |  $schedule lwm:hasDueDate $dueDate .
                     |  $schedule lwm:hasAssignmentDateTimetableEntry $assignmentEntry .
                     |  $schedule lwm:hasAssignmentDate $assignmentDate .
                     |  $schedule lwm:hasGroup $oldGroup .
                     |  $schedule lwm:hasAssignmentAssociation $assignment .
                     |} ;
                   """.stripMargin)
            }
            builder.toString()
          }

          def buildInsert() = {
            val builder = StringBuilder.newBuilder
            builder.append(Vocabulary.defaultPrefixes)
            for (i ← 0 until studentSchedule.size) {
              builder.append(
                s"""
                   |Insert {
                   |${studentSchedule(i)} ?p ?o
                   |}
                   |Where {
                   |${newGroupSchedule(i)} ?p ?o
                   |
                   |filter(?p != rdf:type)
                   |filter(?p != lwm:hasPassed && ?p != lwm:hasAttended)
                   |} ;
                 """.stripMargin)
            }
            builder.toString()
          }

          def buildStudentGroupDeletion() = {
            s"""
               |${Vocabulary.defaultPrefixes}
               |
               | Delete data {
               | $student lwm:memberOf $oldGroup .
               | $oldGroup lwm:hasMember $student .
               | } ;
               | Insert data {
               | $student lwm:memberOf $newGroup .
               | $newGroup lwm:hasMember $student .
               | }
             """.stripMargin
          }

          val megaUpdate = buildDelete() + buildInsert() + buildStudentGroupDeletion()

          val p = Promise[Result]()

          blocking {
            megaUpdate.execUpdate()
            p.success(Redirect(routes.LabworkManagementController.index()))
          }
          p.future
        } else {
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        }
    }
  }
}
