package controllers

import controllers.AssignmentManagementController._
import controllers.StudentsManagement._
import models._
import org.joda.time.{ LocalDate, DateTime }
import org.pegdown.PegDownProcessor
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.libs.json.{ JsBoolean, JsObject }
import play.api.mvc.{ Action, Controller }
import play.twirl.api.Html
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.Vocabulary._
import utils.semantic._
import utils.Global._
import scala.collection.generic.SeqFactory
import scala.concurrent.Future
import scala.reflect.io.Path
import scala.util.control.NonFatal
import java.io._
import org.joda.time._

/**
  * Created by rgiacinto on 20/08/14.
  */
object LabworkManagementController extends Controller with Authentication with TransactionSupport {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import Play.current
  override val system = Akka.system

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      for {
        courses ← Courses.all()
        labworks ← LabWorks.all()
        semesters ← Semesters.all()
      } yield {
        Ok(views.html.labwork_management(semesters.toList, labworks.toList, courses.toList, LabWorkForms.labworkForm))
      }
    }
  }

  def hide() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action(parse.json) { implicit request ⇒
      val student = (request.body \ "student").asOpt[String]
      val labwork = (request.body \ "labwork").asOpt[String]
      val hide = (request.body \ "hide").asOpt[Boolean]
      for {
        s ← student
        l ← labwork
        h ← hide
      } {
        if (h) {
          Students.addHideState(Resource(l), Resource(s))
        } else {
          Students.removeHideState(Resource(l), Resource(s))
        }
      }

      Ok(JsObject(Seq(
        "hidden" -> JsBoolean(hide.getOrElse(false))
      )))
    }
  }

  def edit(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

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
          | <$labworkid> ${LWM.hasGroup} ?o .
          | ?o ${LWM.hasGroupId} ?id
          |} order by asc(?id)
        """.stripMargin

      val associationsQuery =
        s"""
          |select (<$labworkid> as ?s) (${LWM.hasAssignmentAssociation} as ?p) ?o where {
          | <$labworkid> ${LWM.hasAssignmentAssociation} ?o .
          | ?o ${LWM.hasOrderId} ?id .
          |} order by asc(?id)
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
          LabWorkForms.labworkUpdateForm))
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
            LabWork(Resource(labwork.courseId), Resource(labwork.semester))).map { lw ⇒
              createTransaction(session.user, lw.uri, s"New labwork ${lw.uri} created by ${session.user}")
              Redirect(routes.LabworkManagementController.index())
            }
        }
      )
    }
  }

  def labworkRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      LabWorks.delete(Resource(id)).map { lw ⇒
        deleteTransaction(session.user, lw, s"Labwork $lw deleted by ${session.user}")
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

          li.props.get(LWM.isVisibleToStudents) match {
            case None ⇒ li.add(LWM.isVisibleToStudents, StringLiteral("true"))
            case Some(list) ⇒
              list.headOption match {
                case None ⇒ li.add(LWM.isVisibleToStudents, StringLiteral("true"))
                case Some(head) ⇒
                  head.value match {
                    case "true" ⇒
                      li.update(LWM.isVisibleToStudents, head, StringLiteral("false"))
                      modifyTransaction(session.user, li.uri, s"Labwork ${li.uri} visibility changed to false")
                    case "false" ⇒
                      li.update(LWM.isVisibleToStudents, head, StringLiteral("true"))
                      modifyTransaction(session.user, li.uri, s"Labwork ${li.uri} visibility changed to true")
                  }
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
                modifyTransaction(session.user, i.uri, s"Labworl ${i.uri} edited by ${session.user}")
              }
              Future.successful(Redirect(routes.LabworkManagementController.edit(id)))
            }
          )
      }
  }

  def export(labworkid: String, typ: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        val labwork = Individual(Resource(labworkid))

        def groupsFuture(labwork: Resource) = {
          val q = s"""
          |select ?s (${LWM.hasGroupId} as ?p) ?o where {
          | <$labworkid> ${LWM.hasGroup} ?s .
          | ?s ${LWM.hasGroupId} ?o }
          | ORDER BY ASC(?o)
        """.stripMargin

          sparqlExecutionContext.executeQuery(q).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.s)
          }
        }

        def assessmentsFuture(labwork: Resource) = {
          val q = s"""
        Select ?s ?p ?o where {
        $labwork ${LWM.hasGroup} ?group .
        ?group ${LWM.hasMember} ?s .
        ?s ${LWM.hasScheduleAssociation} ?schedule .
        ?schedule ${LWM.hasGroup} ?group .
        ?group ${LWM.hasLabWork} $labwork .
        ?schedule ${LWM.hasAssignmentDate} ?date .
        ?schedule ?p ?o
        filter(?date < '${DateTime.now().toLocalDate.toString}')
        filter((?p = ${LWM.hasPassed} && ?o = "false") || (?p = ${LWM.hasAttended} && ?o = "false") || ?p = ${LWM.hasAlternateScheduleAssociation})
        } order by asc(?s)
        """.stripMargin
          println(q)
          sparqlExecutionContext.executeQuery(q).map { result ⇒
            SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p))
          }
        }
        (for {
          groups ← groupsFuture(labwork.uri)
          assessments ← assessmentsFuture(labwork.uri)
        } yield {
          val groupsWithStudents = groups.map(r ⇒ Individual(r)).map(e ⇒ (e, e.props.getOrElse(LWM.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))))
          val countedData = assessments.groupBy(_._1).map(e ⇒ (Individual(e._1), e._2.count(s ⇒ s._2 == LWM.hasAttended), e._2.count(s ⇒ s._2 == LWM.hasPassed), e._2.count(s ⇒ s._2 == LWM.hasAlternateScheduleAssociation))).toList
          typ match {
            case LabworkExportModes.InternalSchedule        ⇒ Ok(views.html.labwork_exported_groups(labwork, groupsWithStudents.toList))
            case LabworkExportModes.PublicGroupMembersTable ⇒ Ok(views.html.labwork_partial_exported_groups(labwork, groupsWithStudents.toList))
            case LabworkExportModes.PublicSchedule          ⇒ Ok(views.html.labwork_export_publicSchedules(labwork.uri))
            case LabworkExportModes.AssessmentSchedule      ⇒ Ok(views.html.labwork_exported_assessments(countedData.sortBy(s ⇒ s._1.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value)))
          }
        }).recover {
          case NonFatal(e) ⇒
            Redirect(routes.LabworkManagementController.edit(labworkid))
        }
    }
  }

  def exportCSV(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒

        val labwork = Resource(labworkid)

        val builder = new StringBuilder

        def courselabelFuture(labwork: Resource) = {
          val query = s"""
          |select ($labwork as ?s) (${LWM.hasCourse} as ?p) ?o where {
          | $labwork ${LWM.hasCourse} ?course .
          | ?course ${LWM.hasId} ?o }
        """.stripMargin
          sparqlExecutionContext.executeQuery(query).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.o)
          }
        }

        def degreelabelFuture(labwork: Resource) = {
          val query = s"""
          |select ($labwork as ?s) (${LWM.hasDegree} as ?p) ?o where {
          | $labwork ${LWM.hasCourse} ?course .
          | ?course ${LWM.hasDegree} ?degree .
          | ?degree ${LWM.hasId} ?o
          | }
        """.stripMargin
          sparqlExecutionContext.executeQuery(query).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.o)
          }
        }

        def groupsFuture(labwork: Resource) = {
          val groupQuery = s"""
          |select ?s (${LWM.hasGroupId} as ?p) ?o where {
          | $labwork ${LWM.hasGroup} ?s .
          | ?s ${LWM.hasGroupId} ?o }
          | ORDER BY ASC(?o)
        """.stripMargin

          sparqlExecutionContext.executeQuery(groupQuery).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.s)
          }
        }

        for {
          groups ← groupsFuture(labwork)
          courseLabel ← courselabelFuture(labwork)
          degreeLabel ← degreelabelFuture(labwork)
        } yield {
          val writer = {
            val f = new File("./csv")
            if (f.exists()) new PrintWriter(new File(s"./csv/${courseLabel.head}_${degreeLabel.head}.csv"))
            else {
              f.mkdir()
              new PrintWriter(new File(s"./csv/${courseLabel.head}_${degreeLabel.head}.csv"))
            }
          }
          val groupsWithStudents = groups.map(r ⇒ Individual(r)).map(e ⇒ (e, e.props.getOrElse(LWM.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))))

          for (g ← groupsWithStudents) {
            for (s ← g._2) {
              builder.append(s"${g._1.props.getOrElse(LWM.hasGroupId, List(StringLiteral(""))).head.value},${s.props.getOrElse(LWM.hasGmId, List(StringLiteral(""))).head.value},${s.props.getOrElse(FOAF.lastName, List(StringLiteral(""))).head.value},${s.props.getOrElse(FOAF.firstName, List(StringLiteral(""))).head.value},${s.props.getOrElse(LWM.hasRegistrationId, List(StringLiteral(""))).head.value} \n")
            }
          }
          writer.write(builder.toString())
          writer.close()
          Redirect(routes.LabworkManagementController.edit(labworkid))
        }
    }
  }

  def exportAssignment(labwork: String, association: String) = hasPermissions(Permissions.DefaultRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒
        val assI = Individual(Resource(association))

        val assignmentQuery =
          s"""
          |select (${assI.uri} as ?s) (${LWM.hasAssignment} as ?p) ?o where {
          | ${assI.uri} ${LWM.hasAssignment} ?o
          | }
        """.stripMargin
        val assignmentFuture = sparqlExecutionContext.executeQuery(assignmentQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val semesterQuery =
          s"""
          |select (<$labwork> as ?s) (${LWM.hasSemester} as ?p) ?o where {
          | <$labwork> ${LWM.hasSemester} ?sem .
          | ?sem ${RDFS.label} ?o
          | }
        """.stripMargin
        val semesterFuture = sparqlExecutionContext.executeQuery(semesterQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val labQuery =
          s"""
          |select (<$labwork> as ?s) (${RDFS.label} as ?p) ?o where {
          | <$labwork> ${RDFS.label} ?o
          | }
        """.stripMargin
        val labFuture = sparqlExecutionContext.executeQuery(labQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val p = new PegDownProcessor()
        for {
          labLabel ← labFuture
          semLabel ← semesterFuture
          assignment ← assignmentFuture
          a = Individual(assignment.head.asResource().get)
        } yield {
          val orderId = assI.props.getOrElse(LWM.hasOrderId, List(StringLiteral(""))).head.value
          val text = p.markdownToHtml(a.props.getOrElse(LWM.hasText, List(StringLiteral(""))).head.value)
          val hints = p.markdownToHtml(a.props.getOrElse(LWM.hasHints, List(StringLiteral(""))).head.value)
          val goals = p.markdownToHtml(a.props.getOrElse(LWM.hasLearningGoals, List(StringLiteral(""))).head.value)
          val description = p.markdownToHtml(a.props.getOrElse(LWM.hasDescription, List(StringLiteral(""))).head.value)
          val label = a.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.value
          val topics = a.props.getOrElse(LWM.hasTopic, List(StringLiteral(""))).head.value
          Ok(views.html.assignment_ordered_export(labLabel.head.value, semLabel.head.value, orderId, Html(label), Html(description), Html(text), Html(hints), Html(goals), topics))
        }
    }
  }
}
