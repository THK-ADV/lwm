package controllers

import controllers.StudentsManagement._
import models._
import org.joda.time.{ LocalDate, DateTime }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDF, OWL, RDFS, LWM }
import utils.semantic._
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
        labworks ← LabWorks.all()
        semesters ← Semesters.all()
      } yield {
        Ok(views.html.labwork_management(semesters.toList, labworks.toList, courses.toList, LabWorkForms.labworkForm))
      }
    }
  }

  def edit(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { request ⇒

      val labworkIndividual = Individual(Resource(labworkid))

      val labworkDegreeQuery =
        s"""
          |select (<$labworkid> as ?s) (${LWM.hasDegree} as ?p) ?o where {
          | <$labworkid> ${LWM.hasCourse} ?course .
          |  ?course ${LWM.hasDegree} ?o .
          |}
        """.stripMargin

      val labworkCourseQuery =
        s"""
          |select (<$labworkid> as ?s) (${LWM.hasCourse} as ?p) ?o where {
          | <$labworkid> ${LWM.hasCourse} ?o .
          |}
        """.stripMargin

      val groupQuery =
        s"""
          |select (<$labworkid> as ?s) (${LWM.hasGroup} as ?p) ?o where {
          | <$labworkid> ${LWM.hasGroup} ?o
          |}
        """.stripMargin

      val associationsQuery =
        s"""
          |select (<$labworkid> as ?s) (${LWM.hasAssignmentAssociation} as ?p) ?o where {
          | <$labworkid> ${LWM.hasAssignmentAssociation} ?o
          |}
        """.stripMargin

      def allowedAssociationsQuery(course: Resource) =
        s"""
          |select ?s (${RDF.typ} as ?p) (${LWM.Assignment} as ?o) where {
          | ?s ${RDF.typ} ${LWM.Assignment} .
          | ?s ${LWM.hasCourse} $course .
          |}
        """.stripMargin

      val groupsFuture = sparqlExecutionContext.executeQuery(groupQuery).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o)
      }

      val associationsFuture = sparqlExecutionContext.executeQuery(associationsQuery).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o)
      }

      val degreeFuture = sparqlExecutionContext.executeQuery(labworkDegreeQuery).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o)
      }

      val courseFuture = sparqlExecutionContext.executeQuery(labworkCourseQuery).map { result ⇒
        SPARQLTools.statementsFromString(result).map(_.o)
      }

      val allowedAssociationsFutureFuture = for {
        degree ← degreeFuture
        course ← courseFuture

      } yield {
        val q = allowedAssociationsQuery(course.head.asResource().get)

        sparqlExecutionContext.executeQuery(q).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.s)
        }
      }
      for {
        allApplications ← LabworkApplications.all()
        groups ← groupsFuture
        semesters ← Semesters.all()
        courses ← Courses.all()
        course ← courseFuture
        degree ← degreeFuture
        associations ← associationsFuture
        allowedAssociationsFuture ← allowedAssociationsFutureFuture
        allowedAssociations ← allowedAssociationsFuture
      } yield {
        val applications = allApplications.filter(r ⇒ r.props.getOrElse(LWM.hasLabWork, List(Resource(""))).head.value == labworkid)

        Ok(views.html.labwork_information(
          labworkIndividual,
          groups.toList.map(node ⇒ Individual(node.asResource().get)),
          associations.toList.map(node ⇒ Individual(node.asResource().get)),
          applications,
          allowedAssociations.toList.map(node ⇒ Individual(node.asResource().get)),
          semesters, courses,
          AssignmentForms.assignmentAssociationForm,
          LabWorkForms.labworkUpdateForm
        ))
      }
    }
  }

  def labWorkPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      LabWorkForms.labworkForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            labworks ← LabWorks.all()
            courses ← Courses.all()
            semesters ← Semesters.all()
          } yield BadRequest(views.html.labwork_management(semesters.toList, labworks.toList, courses.toList, formWithErrors))
        },
        labwork ⇒ {
          LabWorks.create(
            LabWork(Resource(labwork.courseId), Resource(labwork.semester))).map { _ ⇒
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

  def setVisibility() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val id = (request.body \ "id").as[String]
          val li = Individual(Resource(id))

          li.props(LWM.allowsApplications).map { e ⇒
            e.value match {
              case "true"  ⇒ li.update(LWM.allowsApplications, e, StringLiteral("false"))
              case "false" ⇒ li.update(LWM.allowsApplications, e, StringLiteral("true"))
            }
          }
          Future.successful(Redirect(routes.LabworkManagementController.index()))
      }
  }

  def metaEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          LabWorkForms.labworkUpdateForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                labworks ← LabWorks.all()
                courses ← Courses.all()
                semesters ← Semesters.all()
              } yield BadRequest(views.html.labwork_management(semesters.toList, labworks.toList, courses.toList, LabWorkForms.labworkForm))
            },
            labwork ⇒ {
              val i = Individual(Resource(id))
              for {
                course ← i.props(LWM.hasCourse)
                sD ← i.props(LWM.hasStartDate)
                eD ← i.props(LWM.hasEndDate)
                semester ← i.props(LWM.hasSemester)
                oC = Individual(course.asResource().get)
                nC = Individual(Resource(labwork.courseId))
              } yield {
                i.update(LWM.hasCourse, course, Resource(labwork.courseId))
                i.update(LWM.hasStartDate, sD, DateLiteral(new LocalDate(labwork.startDate)))
                i.update(LWM.hasEndDate, eD, DateLiteral(new LocalDate(labwork.endDate)))
                i.update(LWM.hasSemester, semester, Resource(labwork.semester))
                i.update(RDFS.label, oC.props.getOrElse(RDFS.label, List(StringLiteral(""))).head, nC.props.getOrElse(RDFS.label, List(StringLiteral(""))).head)
              }
              Future.successful(Redirect(routes.LabworkManagementController.edit(id)))
            }
          )
      }
  }

  def export(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      request ⇒
        val i = Individual(Resource(labworkid))

        val groupQuery =
          s"""
          |select ?s (${LWM.hasGroupId} as ?p) ?o where {
          | <$labworkid> ${LWM.hasGroup} ?s .
          | ?s ${LWM.hasGroupId} ?o }
          | ORDER BY ASC(?o)
        """.stripMargin

        val groupsFuture = sparqlExecutionContext.executeQuery(groupQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.s)
        }

        for {
          groups ← groupsFuture
        } yield {
          val groupsWithStudents = groups.map(r ⇒ Individual(r)).map(e ⇒ (e, e.props.getOrElse(LWM.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))))
          Ok(views.html.labwork_exported_groups(i, groupsWithStudents.toList))
        }
    }
  }

}
