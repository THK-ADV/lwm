package controllers

import controllers.LabworkManagementController._
import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic._
import utils.semantic.Vocabulary.{ RDF, LWM, OWL }
import utils.Global._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

object GroupManagementController extends Controller with Authentication {

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
                ScheduleAssociations.create(assoc, studentResource)
              }
            }
            val ig = Individual(groupResource)

            ig.add(LWM.hasMember, studentResource)
            val is = Individual(studentResource)
            is.add(LWM.memberOf, groupResource)

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
                val r = SPARQLBuilder.removeIndividual(statement.o.asResource().get)
                sparqlExecutionContext.executeUpdate(r)
              }

              val ig = Individual(groupResource)
              ig.remove(LWM.hasMember, studentResource)
              val is = Individual(studentResource)
              is.remove(LWM.memberOf, groupResource)
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

          val studentIndividual = Individual(Resource(student.get))
          val oldGroupIndividual = Individual(Resource(oldGroup.get))
          val newGroupIndividual = Individual(Resource(newGroup.get))

          def statementInsertion(entryWithSchedule: Resource, group: Resource) = {
            val query =
              s"""
          select distinct ?s ?p ?o where {
          $group ${LWM.hasScheduleAssociation} ?newGroup .
          $entryWithSchedule ${LWM.hasScheduleAssociation} ?s .
          ?newGroup ${LWM.hasAssignmentAssociation} ?assoc .
          ?s ${LWM.hasAssignmentAssociation} ?assoc .
          ?newGroup ?p ?o .
          filter(?p != ${RDF.typ} && ?p != ${LWM.hasAssignmentAssociation} && ?p != ${OWL.NamedIndividual})
          }
          """.stripMargin

            sparqlExecutionContext.executeQuery(query).map { result ⇒
              val statements = SPARQLTools.statementsFromString(result).map(r ⇒ (r.s, r.p, r.o))
              val update = buildQuery("insert", statements)
              //println(s"Insert: \n$update")
              sparqlExecutionContext.executeUpdate(update)
            }
          }
          def statementRemoval(entryWithSchedule: Resource, group: Resource) = {
            val query =
              s"""
          select ?s ?p ?o where {
          $entryWithSchedule ${LWM.hasScheduleAssociation} ?s .
          ?s ${LWM.hasGroup} $group .
          ?s ?p ?o .
          filter(?p != ${RDF.typ} && ?p != ${LWM.hasAssignmentAssociation} && ?p != ${OWL.NamedIndividual} && ?p != ${LWM.hasPassed} && ?p != ${LWM.hasAttended})
          }
          """.stripMargin

            sparqlExecutionContext.executeQuery(query).map { result ⇒
              val statements = SPARQLTools.statementsFromString(result).map(r ⇒ (r.s, r.p, r.o))
              val update = buildQuery("delete", statements)
              //println(s"Remove: \n$update")
              sparqlExecutionContext.executeUpdate(update)
            }
          }

          def buildQuery(op: String, map: Seq[(Resource, Property, RDFNode)]): String = {
            val builder = new StringBuilder
            if (op == "delete") builder.append("DELETE DATA { ") else builder.append("INSERT DATA { ")
            map.foreach {
              e ⇒
                builder.append(s"${e._1} ${e._2} ${e._3.toQueryString} . ")
            }
            builder.deleteCharAt(builder.length - 2)
            builder.append("}")
            builder.toString()
          }
          for {
            deleteStage1 ← statementRemoval(studentIndividual.uri, oldGroupIndividual.uri)
            deleteStage2 ← deleteStage1
            insertStage1 ← statementInsertion(studentIndividual.uri, newGroupIndividual.uri)
            insertStage2 ← insertStage1
          } yield {
            studentIndividual.update(LWM.memberOf, oldGroupIndividual.uri, newGroupIndividual.uri)
            oldGroupIndividual.remove(LWM.hasMember, studentIndividual.uri)
            newGroupIndividual.add(LWM.hasMember, studentIndividual.uri)
            Redirect(routes.GroupManagementController.index(oldGroupIndividual.props.getOrElse(LWM.hasLabWork, List(Resource(""))).head.value, oldGroupIndividual.uri.value))
          }
        } else {
          Future.successful(Redirect(routes.LabworkManagementController.index()))
        }
    }
  }
}
