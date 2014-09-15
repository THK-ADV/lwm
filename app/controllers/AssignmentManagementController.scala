package controllers

import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ Literal, Resource, Individual }
import scala.concurrent.ExecutionContext.Implicits.global
import utils.Global._

import scala.concurrent.Future

/**
  * Created by root on 9/13/14.
  */
object AssignmentManagementController extends Controller with Authentication {

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          for {
            assignments ← Assignments.all()
            courses ← Courses.all()
          } yield {
            assignments.foreach {
              a ⇒
                println(Individual(Resource(a.props.getOrElse(LWM.hasCourse, List(Resource(""))).head.value)).props.getOrElse(LWM.hasDegree, List(Literal(""))).head.value)
            }
            Ok(views.html.assignmentManagement(assignments.map(_.uri), courses, AssignmentForms.assignmentForm, AssignmentForms.assignmentSolutionForm))
          }
      }
  }

  def assignmentPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentManagement(assignments.map(_.uri), courses, formWithErrors, AssignmentForms.assignmentSolutionForm))
              }
            },
            a ⇒
              Assignments.create(Assignment(a.id, a.description, a.text, a.topics.split(",").toList, a.courses)).map { _ ⇒
                Redirect(routes.AssignmentManagementController.index())
              }
          )
      }
  }

  def assignmentRemoval() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          Assignments.delete(Resource(id)).map { _ ⇒
            Redirect(routes.AssignmentManagementController.index())
          }
      }
  }

  def assignmentSolutionPost(assignmentid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentSolutionForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentManagement(assignments.map(_.uri), courses, AssignmentForms.assignmentForm, formWithErrors))
              }
            },
            a ⇒
              AssignmentSolutions.create(AssignmentSolution(a.name, a.text, Resource(assignmentid))).map { _ ⇒
                Redirect(routes.AssignmentManagementController.index())
              }
          )
      }
  }

  //TODO: ADD ASSOCIATION
  def labAssignmentIndex(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          for (courses ← Courses.all()) yield {
            Ok(views.html.assignmentLabworkManagement(Resource(labworkid), Individual(Resource(labworkid)).props(LWM.hasAssignmentAssociation).map(_.asResource().get), courses, AssignmentForms.assignmentAssociationForm, AssignmentForms.assignmentSolutionForm))
          }
      }
  }

  def labAssignmentPost(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentAssociationForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentLabworkManagement(Resource(labworkid), Individual(Resource(labworkid)).props(LWM.hasAssignmentAssociation).map(_.asResource().get), courses, formWithErrors, AssignmentForms.assignmentSolutionForm))
              }
            },
            a ⇒ {
              AssignmentAssociations.create(AssignmentAssociation(Resource(a.assignment), Resource(labworkid), a.preparationTime)).map { _ ⇒
                Redirect(routes.LabworkManagementController.edit(labworkid))
              }
            }
          )
      }
  }

  def bindAssignment(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          AssignmentForms.assignmentAssociationForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                assignments ← Assignments.all()
                courses ← Courses.all()
              } yield {
                BadRequest(views.html.assignmentLabworkManagement(Resource(labworkid), Individual(Resource(labworkid)).props(LWM.hasAssignmentAssociation).map(_.asResource().get), courses, formWithErrors, AssignmentForms.assignmentSolutionForm))
              }
            },
            a ⇒ {
              val i = Individual(Resource(associationid))
              i.add(LWM.hasAssignment, Resource(a.assignment))
              i.add(LWM.hasPreparationTime, Literal(s"${a.preparationTime}"))
              Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
            }
          )
      }
  }

  def bindRemoval(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val labworkid = (request.body \ "lId").as[String]
          val associationid = (request.body \ "aId").as[String]
          val i = Individual(Resource(associationid))
          i.remove(LWM.hasAssignment, i.props.getOrElse(LWM.hasAssignment, List.empty[Resource]).head)
          Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
      }
  }

  def bindEdit(labworkid: String, associationid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒
        val ia = Individual(Resource(associationid))
        val ot = ia.props.getOrElse(LWM.hasPreparationTime, List(Literal(""))).head
        val nt = (request.body \ "time").as[String]
        val gid = (request.body \ "group").as[String]
        ia.update(LWM.hasPreparationTime, ot, Literal(nt))
        Future.successful(Redirect(routes.GroupManagementController.index(labworkid, gid)))
      }
  }

}
