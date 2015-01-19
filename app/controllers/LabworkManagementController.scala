package controllers

import java.net.URLDecoder

import controllers.AssignmentManagementController._
import controllers.StudentsManagement._
import models._
import org.joda.time.{ LocalDate, DateTime }
import org.pegdown.PegDownProcessor
import play.api.Play
import play.api.libs.concurrent.Akka
import play.api.libs.json.{ JsString, JsBoolean, JsObject }
import play.api.mvc.{ Action, Controller }
import play.twirl.api.Html
import utils.Security.Authentication
import utils.{ QueryHost, TransactionSupport }
import utils.semantic.Vocabulary._
import utils.semantic._
import utils.Global._
import scala.collection.generic.SeqFactory
import scala.concurrent.Future
import scala.reflect.io.Path
import scala.util.control.NonFatal
import java.io._
import org.joda.time._
import utils.Implicits._

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
        courseResources ← Courses.all()
        labworks ← LabWorks.all()
        semesterResources ← Semesters.all()
        courses = courseResources.map(c ⇒ Individual(c))
        semesters = semesterResources.map(s ⇒ Individual(s))
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
          |select (<$labworkid> as ?s) (${lwm.hasDegree} as ?p) ?o where {
          | <$labworkid> ${lwm.hasCourse} ?course .
          |  ?course ${lwm.hasDegree} ?o .
          |}
        """.stripMargin

      val labworkCourseQuery =
        s"""
          |select (<$labworkid> as ?s) (${lwm.hasCourse} as ?p) ?o where {
          | <$labworkid> ${lwm.hasCourse} ?o .
          |}
        """.stripMargin

      val groupQuery =
        s"""
          |select (<$labworkid> as ?s) (${lwm.hasGroup} as ?p) ?o where {
          | <$labworkid> ${lwm.hasGroup} ?o .
          | ?o ${lwm.hasGroupId} ?id
          |} order by asc(?id)
        """.stripMargin

      val associationsQuery =
        s"""
          |select (<$labworkid> as ?s) (${lwm.hasAssignmentAssociation} as ?p) ?o where {
          | <$labworkid> ${lwm.hasAssignmentAssociation} ?o .
          | ?o ${lwm.hasOrderId} ?id .
          |} order by asc(?id)
        """.stripMargin

      def allowedAssociationsQuery(course: Resource) =
        s"""
          |select ?s (${rdf.typ} as ?p) (${lwm.Assignment} as ?o) where {
          | ?s ${rdf.typ} ${lwm.Assignment} .
          | ?s ${lwm.hasCourse} $course .
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
        val applications = allApplications.filter(r ⇒ r.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head.value == labworkid)

        Ok(views.html.labwork_information(
          labworkIndividual,
          groups.toList.map(node ⇒ Individual(node.asResource().get)),
          associations.toList.map(node ⇒ Individual(node.asResource().get)),
          applications,
          allowedAssociations.toList.map(node ⇒ Individual(node.asResource().get)),
          semesters.map(s ⇒ Individual(s)), courses.map(c ⇒ Individual(c)),
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
            courseResources ← Courses.all()
            labworks ← LabWorks.all()
            semesterResources ← Semesters.all()
            courses = courseResources.map(c ⇒ Individual(c))
            semesters = semesterResources.map(s ⇒ Individual(s))
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

          val typ =
            s"""
               |${Vocabulary.defaultPrefixes}
               |
               | Select (${li.uri} as ?s) (rdf:type as ?p) ?o where {
               |     ${li.uri} rdf:type ?o .
               |     filter(?o != owl:NamedIndividual)
               | }
             """.stripMargin.execSelect().head.data("o").asResource().getURI

          Resource(typ) match {
            case lwm.LabWork               ⇒ setProp(lwm.allowsApplications)
            case lwm.AssignmentAssociation ⇒ setProp(lwm.isVisibleToStudents)
          }

          def setProp(property: Property) = {
            li.props.get(property) match {
              case None ⇒ li.add(property, StringLiteral("true"))
              case Some(list) ⇒
                list.headOption match {
                  case None ⇒ li.add(property, StringLiteral("true"))
                  case Some(head) ⇒
                    head.value match {
                      case "true" ⇒
                        li.update(property, head, StringLiteral("false"))
                        modifyTransaction(session.user, li.uri, s"Labwork ${li.uri} visibility changed to false")
                      case "false" ⇒
                        li.update(property, head, StringLiteral("true"))
                        modifyTransaction(session.user, li.uri, s"Labwork ${li.uri} visibility changed to true")
                    }
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
                courseResources ← Courses.all()
                labworks ← LabWorks.all()
                semesterResources ← Semesters.all()
                courses = courseResources.map(c ⇒ Individual(c))
                semesters = semesterResources.map(s ⇒ Individual(s))
              } yield BadRequest(views.html.labwork_management(semesters.toList, labworks.toList, courses.toList, LabWorkForms.labworkForm))
            },
            labwork ⇒ {
              val i = Individual(Resource(id))
              for {
                course ← i.props(lwm.hasCourse)
                sD ← i.props(lwm.hasStartDate)
                eD ← i.props(lwm.hasEndDate)
                semester ← i.props(lwm.hasSemester)
                oC = Individual(course.asResource().get)
                nC = Individual(Resource(labwork.courseId))
              } yield {
                i.update(lwm.hasCourse, course, Resource(labwork.courseId))
                i.update(lwm.hasStartDate, sD, DateLiteral(new LocalDate(labwork.startDate)))
                i.update(lwm.hasEndDate, eD, DateLiteral(new LocalDate(labwork.endDate)))
                i.update(lwm.hasSemester, semester, Resource(labwork.semester))
                i.update(rdfs.label, oC.props.getOrElse(rdfs.label, List(StringLiteral(""))).head, nC.props.getOrElse(rdfs.label, List(StringLiteral(""))).head)
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
          |select ?s (${lwm.hasGroupId} as ?p) ?o where {
          | <$labworkid> ${lwm.hasGroup} ?s .
          | ?s ${lwm.hasGroupId} ?o }
          | ORDER BY ASC(?o)
        """.stripMargin

          sparqlExecutionContext.executeQuery(q).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.s)
          }
        }

        def assessmentsFuture(labwork: Resource) = {
          val q = s"""
        Select ?s ?p ?o where {
        $labwork ${lwm.hasGroup} ?group .
        ?group ${lwm.hasMember} ?s .
        ?s ${lwm.hasScheduleAssociation} ?schedule .
        ?schedule ${lwm.hasGroup} ?group .
        ?group ${lwm.hasLabWork} $labwork .
        ?schedule ${lwm.hasAssignmentDate} ?date .
        ?schedule ?p ?o
        filter(?date < '${DateTime.now().toLocalDate.toString}')
        filter((?p = ${lwm.hasPassed} && ?o = "false") || (?p = ${lwm.hasAttended} && ?o = "false") || ?p = ${lwm.hasAlternateScheduleAssociation})
        } order by asc(?s)
        """.stripMargin
          sparqlExecutionContext.executeQuery(q).map { result ⇒
            SPARQLTools.statementsFromString(result).map(e ⇒ (e.s, e.p))
          }
        }
        val emails = createFile(labwork.uri)
        (for {
          groups ← groupsFuture(labwork.uri)
          assessments ← assessmentsFuture(labwork.uri)
        } yield {
          val groupsWithStudents = groups.map(r ⇒ Individual(r)).map(e ⇒ (e, e.props.getOrElse(lwm.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))))
          val countedData = assessments.groupBy(_._1).map(e ⇒ (Individual(e._1), e._2.count(s ⇒ s._2 == lwm.hasAttended), e._2.count(s ⇒ s._2 == lwm.hasPassed), e._2.count(s ⇒ s._2 == lwm.hasAlternateScheduleAssociation))).toList
          typ match {
            case LabworkExportModes.InternalSchedule        ⇒ Ok(views.html.labwork_exported_groups(labwork, groupsWithStudents.toList))
            case LabworkExportModes.PublicGroupMembersTable ⇒ Ok(views.html.labwork_partial_exported_groups(labwork, groupsWithStudents.toList))
            case LabworkExportModes.PublicSchedule          ⇒ Ok(views.html.labwork_export_publicSchedules(labwork.uri))
            case LabworkExportModes.AssessmentSchedule      ⇒ Ok(views.html.labwork_exported_assessments(countedData.sortBy(s ⇒ s._1.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value)))
            case LabworkExportModes.LabworkGraduates        ⇒ Ok(views.html.labwork_exported_graduates(labwork.uri))
            case LabworkExportModes.EmailList               ⇒ Ok.sendFile(emails).withHeaders(CACHE_CONTROL -> "max-age=3600", CONTENT_DISPOSITION -> "attachment; filename=emails.csv", CONTENT_TYPE -> "application/x-download")
          }
        }).recover {
          case NonFatal(e) ⇒
            Redirect(routes.LabworkManagementController.edit(labworkid))
        }
    }
  }

  private def createFile(labwork: Resource): File = {
    val path = "./csv/emails.csv"
    val writer = {
      val f = new File("./csv")
      if (f.exists()) new PrintWriter(new File(path))
      else {
        f.mkdir()
        new PrintWriter(new File(path))
      }
    }

    val emails = s"""
       |${Vocabulary.defaultPrefixes}
       |
       | Select ?email ?gmId {
       | ?student lwm:memberOf ?group .
       | ?group lwm:hasLabWork $labwork .
       | optional { ?student foaf:mbox ?email }
       | ?student lwm:hasGmId ?gmId
       |
       | }
     """.stripMargin.execSelect().map { qs ⇒

      val gmId = qs.data("gmId").asLiteral().getString
      val email = URLDecoder.decode(qs.data("email").asLiteral().getString, "UTF-8")

      s"$gmId@gm.fh-koeln.de" :: email :: Nil
    }.flatten.mkString("\n")
    writer.write(emails)
    writer.close()
    new File(path)
  }

  def exportCSV(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async {
      implicit request ⇒

        val labwork = Resource(labworkid)

        val builder = new StringBuilder

        def courselabelFuture(labwork: Resource) = {
          val query = s"""
          |select ($labwork as ?s) (${lwm.hasCourse} as ?p) ?o where {
          | $labwork ${lwm.hasCourse} ?course .
          | ?course ${lwm.hasId} ?o }
        """.stripMargin
          sparqlExecutionContext.executeQuery(query).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.o)
          }
        }

        def degreelabelFuture(labwork: Resource) = {
          val query = s"""
          |select ($labwork as ?s) (${lwm.hasDegree} as ?p) ?o where {
          | $labwork ${lwm.hasCourse} ?course .
          | ?course ${lwm.hasDegree} ?degree .
          | ?degree ${lwm.hasId} ?o
          | }
        """.stripMargin
          sparqlExecutionContext.executeQuery(query).map { result ⇒
            SPARQLTools.statementsFromString(result).map(_.o)
          }
        }

        def groupsFuture(labwork: Resource) = {
          val groupQuery = s"""
          |select ?s (${lwm.hasGroupId} as ?p) ?o where {
          | $labwork ${lwm.hasGroup} ?s .
          | ?s ${lwm.hasGroupId} ?o }
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
          val groupsWithStudents = groups.map(r ⇒ Individual(r)).map(e ⇒ (e, e.props.getOrElse(lwm.hasMember, Nil).map(r ⇒ Individual(Resource(r.value)))))

          for (g ← groupsWithStudents) {
            for (s ← g._2) {
              builder.append(s"${g._1.props.getOrElse(lwm.hasGroupId, List(StringLiteral(""))).head.value},${s.props.getOrElse(lwm.hasGmId, List(StringLiteral(""))).head.value},${s.props.getOrElse(foaf.lastName, List(StringLiteral(""))).head.value},${s.props.getOrElse(foaf.firstName, List(StringLiteral(""))).head.value},${s.props.getOrElse(lwm.hasRegistrationId, List(StringLiteral(""))).head.value} \n")
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
          |select (${assI.uri} as ?s) (${lwm.hasAssignment} as ?p) ?o where {
          | ${assI.uri} ${lwm.hasAssignment} ?o
          | }
        """.stripMargin
        val assignmentFuture = sparqlExecutionContext.executeQuery(assignmentQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val semesterQuery =
          s"""
          |select (<$labwork> as ?s) (${lwm.hasSemester} as ?p) ?o where {
          | <$labwork> ${lwm.hasSemester} ?sem .
          | ?sem ${rdfs.label} ?o
          | }
        """.stripMargin
        val semesterFuture = sparqlExecutionContext.executeQuery(semesterQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val labQuery =
          s"""
          |select (<$labwork> as ?s) (${rdfs.label} as ?p) ?o where {
          | <$labwork> ${rdfs.label} ?o
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
          val orderId = assI.props.getOrElse(lwm.hasOrderId, List(StringLiteral(""))).head.value
          val text = p.markdownToHtml(a.props.getOrElse(lwm.hasText, List(StringLiteral(""))).head.value)
          val hints = p.markdownToHtml(a.props.getOrElse(lwm.hasHints, List(StringLiteral(""))).head.value)
          val goals = p.markdownToHtml(a.props.getOrElse(lwm.hasLearningGoals, List(StringLiteral(""))).head.value)
          val description = p.markdownToHtml(a.props.getOrElse(lwm.hasDescription, List(StringLiteral(""))).head.value)
          val label = a.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value
          val topics = a.props.getOrElse(lwm.hasTopic, List(StringLiteral(""))).head.value
          Ok(views.html.assignment_ordered_export(labLabel.head.value, semLabel.head.value, orderId, Html(label), Html(description), Html(text), Html(hints), Html(goals), topics))
        }
    }
  }

  def labworkApproval(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒

    Action.async(parse.json) {
      implicit request ⇒
        import play.api.libs.json._

        val labwork = Resource(labworkid)
        val maybeStudent = (request.body \ "student").asOpt[String]

        def askForApproval(student: Resource, labwork: Resource) = {
          s"""
            |${Vocabulary.defaultPrefixes}
            |
            | ASK {
            |  $student lwm:hasLabworkApproval $labwork
            | }
          """.stripMargin.executeAsk()
        }

        if (maybeStudent.isDefined) {
          val student = Individual(Resource(maybeStudent.get))
          Students.getLabworkApprovalProperty(student.uri, labwork) match {

            case None ⇒ student.add(lwm.hasLabworkApproval, labwork)
            case Some(property) ⇒
              if (askForApproval(student.uri, labwork)) {
                student.remove(lwm.hasLabworkApproval, labwork)
                student.add(lwm.hasLabworkDisapproval, labwork)
              } else {
                student.remove(lwm.hasLabworkDisapproval, labwork)
                student.add(lwm.hasLabworkApproval, labwork)
              }
          }

          val json = JsObject(Seq(
            ("id", JsNumber(student.uri.hashCode())),
            ("approvedState", JsBoolean(askForApproval(student.uri, labwork)))
          ))

          Future.successful(Ok(json))
        } else {
          Future.successful(Redirect(routes.LabworkManagementController.edit(labworkid)))
        }
    }
  }
}
