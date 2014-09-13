package controllers

import controllers.StudentsManagement._
import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ OWL, RDFS, LWM }
import utils.semantic.{ RDFNode, Individual, Resource }
import utils.Global._
import scala.concurrent.Future

/**
  * Created by rgiacinto on 20/08/14.
  */
object LabworkManagementController extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      for {
        courses ← Courses.all()
        degrees ← Degrees.all()
        labworks ← LabWorks.all()
        semesters ← Semesters.all()
      } yield {
        Ok(views.html.labwork_management(semesters.toList, labworks.toList, degrees.toList, courses.toList, LabWorkForms.labworkForm))
      }
    }
  }

  //TODO: ADD ASSIGNMENTS
  def edit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒
      val labworkIndividual = Individual(Resource(id))
      val groups = labworkIndividual.props.getOrElse(LWM.hasGroup, List.empty[Resource]).map(r ⇒ Individual(r.asResource().get))
      Future.successful(Ok(views.html.labWorkInformation(labworkIndividual, groups, Nil)))
    }
  }

  def labWorkPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      LabWorkForms.labworkForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            labworks ← LabWorks.all()
            courses ← Courses.all()
            degrees ← Degrees.all()
            semesters ← Semesters.all()
          } yield BadRequest(views.html.labwork_management(semesters.toList, labworks.toList, degrees.toList, courses.toList, formWithErrors))
        },
        labwork ⇒ {
          LabWorks.create(labwork).map { _ ⇒
            Redirect(routes.LabworkManagementController.index())
          }
        }
      )
    }
  }

  def labworkRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      LabWorks.delete(Resource(id)).map { _ ⇒
        Redirect(routes.LabworkManagementController.index())
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
            val ig = Individual(groupResource)
            ig.add(LWM.hasMember, studentResource)(lwmGraph)
            val is = Individual(studentResource)
            is.add(LWM.memberOf, groupResource)(lwmGraph)
          }
          Redirect(routes.LabworkManagementController.index())
        }
      }
    }
  }
}
