package controllers

import java.util.Scanner

import controllers.LabworkManagementController._
import models._
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic._
import utils.semantic.Vocabulary.{ rdfs, rdf, lwm, owl }
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
        val student = (request.body \ "student").asOpt[String]
        val oldGroup = (request.body \ "old").asOpt[String]
        val newGroup = (request.body \ "new").asOpt[String]
        if (student.isDefined && oldGroup.isDefined && newGroup.isDefined) {

          def procure(id: Option[String]): String = {
            Individual(Resource(id.get)).props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value
          }

          def assData(entryWithSchedule: Resource) = {
            val q =
              s"""
                 Select ?s ?p ?o where {
                 $entryWithSchedule ${lwm.hasScheduleAssociation} ?schedule .
                  ?schedule ${lwm.hasAssignmentAssociation} ?s .
                  ?schedule ?p ?o
                 filter(?p != ${rdf.typ} && ?p != ${lwm.hasAssignmentAssociation} && ?p != ${owl.NamedIndividual})
                 } order by asc(?s) asc(?p)
               """.stripMargin

            sparqlExecutionContext.executeQuery(q).map { result ⇒
              SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p, e.o))
            }
          }
          def scheduleLink(student: Resource, oldGroup: Resource) = {
            val q = s"""
                 Select ?s (${lwm.hasAssignmentAssociation} as ?p) ?o where {
                 $student ${lwm.hasScheduleAssociation} ?s .
                  ?s ${lwm.hasGroup} $oldGroup .
                  ?s ${lwm.hasAssignmentAssociation} ?o
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
                  $student ${lwm.hasScheduleAssociation} ?s .
                  ?s ${lwm.hasGroup} $oldGroup .
                  ?s ?p ?o
                  filter(?p != ${lwm.hasAssignmentAssociation} && ?p != ${owl.NamedIndividual} && ?p != ${rdf.typ} && ?p != ${lwm.hasPassed} && ?p != ${lwm.hasAttended})
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

            studentIndividual.update(lwm.memberOf, Resource(oldGroup.get), Resource(newGroup.get))
            oldGroupIndividual.remove(lwm.hasMember, Resource(student.get))
            newGroupIndividual.add(lwm.hasMember, Resource(student.get))
            modifyTransaction(session.user, studentIndividual.uri, s"Student ${studentIndividual.props.get(rdfs.label).map(_.headOption.map(_.toString)).getOrElse("")} moved to new Group ${newGroupIndividual.props.get(rdfs.label).map(_.headOption.map(_.toString)).getOrElse("")} by ${session.user}")
            Redirect(routes.GroupManagementController.index(oldGroupIndividual.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head.value, oldGroupIndividual.uri.value))
          }
        } else {
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        }
    }
  }
}
