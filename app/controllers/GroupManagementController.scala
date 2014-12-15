package controllers

import java.util.Scanner

import controllers.LabworkManagementController._
import models._
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic._
import utils.semantic.Vocabulary.{ RDFS, RDF, LWM, OWL }
import utils.Global._
import scala.concurrent.Future
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
         select ?s (${LWM.hasGroupId} as ?p) ?o where {
          ?s ${RDF.typ} ${LWM.Group} .
          ?s ${LWM.hasLabWork} <$labworkId> .
          ?s ${LWM.hasGroupId} ?o
          }
          ORDER BY ASC(?o)
            """.stripMargin

        val futureGroups = sparqlExecutionContext.executeQuery(query).map { result ⇒
          SPARQLTools.statementsFromString(result).map(r ⇒ Individual(r.s))
        }

        futureGroups.map { g ⇒
          val lI = Individual(Resource(labworkId))
          val gI = Individual(Resource(groupId))
          val s = gI.props.getOrElse(LWM.hasMember, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          val a = lI.props.getOrElse(LWM.hasAssignmentAssociation, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          Ok(views.html.groups_detail_management(lI, gI, s, g.toList, a))
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
          isStudent ← Students.isStudent(studentResource)
          isGroup ← LabworkGroups.isLabWorkGroup(groupResource)
        } yield {
          if (isStudent && isGroup) {
            ScheduleAssociations.getForGroup(groupResource).map { assocs ⇒
              assocs.map { assoc ⇒
                ScheduleAssociations.create(assoc, studentResource).map { i ⇒
                  createTransaction(session.user, i.uri, s"Schedule ${i.uri} created for Student $studentResource by ${session.user}")
                }
              }
            }
            val ig = Individual(groupResource)

            ig.add(LWM.hasMember, studentResource)
            createTransaction(session.user, ig.uri, s"Student $studentResource added to Group ${ig.uri} by ${session.user}")
            val is = Individual(studentResource)
            is.add(LWM.memberOf, groupResource)
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
         select ($student as ?s) (${LWM.hasApplication} as ?p) (?application as ?o) where {
          $student ${LWM.hasPendingApplication} ?application .
          ?application ${LWM.hasLabWork} ?labwork .
          ?labwork ${LWM.hasGroup} $group .
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
            isStudent ← Students.isStudent(studentResource)
            isGroup ← LabworkGroups.isLabWorkGroup(groupResource)
          } yield {
            if (isStudent && isGroup) {
              val query =
                s"""
                   |select ($studentResource as ?s) (${LWM.hasScheduleAssociation} as ?p) (?ass as ?o) where {
                   | $studentResource ${LWM.hasScheduleAssociation} ?ass .
                   | ?ass ${LWM.hasGroup} $groupResource
                   |}
                 """.stripMargin

              SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
                sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(statement.o.asResource().get))
              }

              val ig = Individual(groupResource)
              ig.remove(LWM.hasMember, studentResource)
              val is = Individual(studentResource)
              is.remove(LWM.memberOf, groupResource)
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
        val student = (request.body \ "student").asOpt[String]
        val oldGroup = (request.body \ "old").asOpt[String]
        val newGroup = (request.body \ "new").asOpt[String]
        if (student.isDefined && oldGroup.isDefined && newGroup.isDefined) {

          def procure(id: Option[String]): String = {
            Individual(Resource(id.get)).props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value
          }
          println(s"P: ${procure(student)} - ${student.get}\nO: ${procure(oldGroup)} - ${oldGroup.get}\nN: ${procure(newGroup)} - ${newGroup.get}")

          def assData(entryWithSchedule: Resource) = {
            val q =
              s"""
                 Select ?s ?p ?o where {
                 $entryWithSchedule ${LWM.hasScheduleAssociation} ?schedule .
                  ?schedule ${LWM.hasAssignmentAssociation} ?s .
                  ?schedule ?p ?o
                 filter(?p != ${RDF.typ} && ?p != ${LWM.hasAssignmentAssociation} && ?p != ${OWL.NamedIndividual})
                 } order by asc(?s) asc(?p)
               """.stripMargin

            sparqlExecutionContext.executeQuery(q).map { result ⇒
              SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p, e.o))
            }
          }
          def scheduleLink(student: Resource, oldGroup: Resource) = {
            val q = s"""
                 Select ?s (${LWM.hasAssignmentAssociation} as ?p) ?o where {
                 $student ${LWM.hasScheduleAssociation} ?s .
                  ?s ${LWM.hasGroup} $oldGroup .
                  ?s ${LWM.hasAssignmentAssociation} ?o
                  } order by asc(?o)
               """.stripMargin

            sparqlExecutionContext.executeQuery(q).map { result ⇒
              SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p, e.o))
            }
          }
          def removeStatements(student: Resource, oldGroup: Resource) = {
            val u =
              s"""
                  Delete {
                  ?s ?p ?o
                  } where {
                  $student ${LWM.hasScheduleAssociation} ?s .
                  ?s ${LWM.hasGroup} $oldGroup .
                  ?s ?p ?o
                  filter(?p != ${LWM.hasAssignmentAssociation} && ?p != ${OWL.NamedIndividual} && ?p != ${RDF.typ} && ?p != ${LWM.hasPassed} && ?p != ${LWM.hasAttended})
                  }
                """.stripMargin
            sparqlExecutionContext.executeUpdate(u)
          }

          def insertStatements(fpiece: Seq[(Resource, Property, RDFNode)], spiece: Seq[(Resource, Property, RDFNode)]) = {
            val builder = StringBuilder.newBuilder
            builder.append("Insert data { ")
            fpiece.map {
              head ⇒
                spiece.map {
                  tail ⇒
                    if (head._3.value == tail._1.value) {
                      builder.append(s"${head._1} ${tail._2} ${if (tail._3.value.contains("http")) tail._3; else s"'${tail._3}'"} . ")
                    }
                }
            }
            val u = builder.dropRight(2).append("}").toString()

            sparqlExecutionContext.executeUpdate(u)
          }

          for {
            studentAssocs ← scheduleLink(Resource(student.get), Resource(oldGroup.get))
            groupAssocs ← assData(Resource(newGroup.get))
            removal ← removeStatements(Resource(student.get), Resource(oldGroup.get))
            add ← insertStatements(studentAssocs, groupAssocs)
          } yield {

            val studentIndividual = Individual(Resource(student.get))
            val oldGroupIndividual = Individual(Resource(oldGroup.get))
            val newGroupIndividual = Individual(Resource(newGroup.get))

            studentIndividual.update(LWM.memberOf, Resource(oldGroup.get), Resource(newGroup.get))
            oldGroupIndividual.remove(LWM.hasMember, Resource(student.get))
            newGroupIndividual.add(LWM.hasMember, Resource(student.get))
            modifyTransaction(session.user, studentIndividual.uri, s"Student ${studentIndividual.props.get(RDFS.label).map(_.headOption.map(_.toString)).getOrElse("")} moved to new Group ${newGroupIndividual.props.get(RDFS.label).map(_.headOption.map(_.toString)).getOrElse("")} by ${session.user}")
            Redirect(routes.GroupManagementController.index(oldGroupIndividual.props.getOrElse(LWM.hasLabWork, List(Resource(""))).head.value, oldGroupIndividual.uri.value))
          }
        } else {
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        }
    }
  }
}
