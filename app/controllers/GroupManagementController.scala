package controllers

import controllers.LabworkManagementController._
import models.{ LabworkApplications, LabworkGroups, Students }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ SPARQLBuilder, SPARQLTools, Individual, Resource }
import utils.semantic.Vocabulary.{ LWM }
import utils.Global._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by root on 9/15/14.
  */
object GroupManagementController extends Controller with Authentication {

  def index(labworkId: String, groupId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          val lI = Individual(Resource(labworkId))
          val gI = Individual(Resource(groupId))
          val s = gI.props.getOrElse(LWM.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))
          val a = lI.props.getOrElse(LWM.hasAssignmentAssociation, Nil).map(r ⇒ Individual(Resource(r.value)))
          Future.successful(Ok(views.html.groups_detail_management(lI, gI, s, a)))

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

            ig.props.get(LWM.hasScheduleAssociation).map { assocs ⇒
              assocs.foreach { ass ⇒
                is.add(LWM.hasScheduleAssociation, ass)
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
}
