package models

import java.util.Date

import org.joda.time.DateTime
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class LabWork(groupCount: Int, assignmentCount: Int, courseId: String, semester: String, startDate: DateTime, endDate: DateTime)
case class LabWorkFormModel(groupCount: Int, assignmentCount: Int, courseId: String, semester: String, startDate: Date, endDate: Date)

case class LabWorkApplication(courseID: String, gmID: String)

// TODO course id should be courseResourceURI

object LabWorkForms {

  import play.api.data.Forms._
  import play.api.data._

  val labWorkApplicationForm = Form(
    mapping(
      "courseID" -> nonEmptyText,
      "gmID" -> nonEmptyText
    )(LabWorkApplication.apply)(LabWorkApplication.unapply)
  )

  val labworkForm = Form(
    mapping(
      "groupCount" -> number(min = 1),
      "assignmentCount" -> number(min = 1),
      "courseId" -> nonEmptyText,
      "semester" -> nonEmptyText,
      "startDate" -> date,
      "endDate" -> date
    )(LabWorkFormModel.apply)(LabWorkFormModel.unapply)
  )

}

/**
  * Praktika
  */
object LabWorks {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(labWork: LabWork): Future[Individual] = {
    val courseIndividual = Individual(Resource(labWork.courseId))
    val resource = ResourceUtils.createResource(lwmNamespace)
    val timetable = Timetables.create(Timetable(resource, labWork.startDate, labWork.endDate))

    val label = courseIndividual.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.asLiteral().get

    val statements = List(
      Statement(resource, RDF.typ, LWM.LabWork),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, RDFS.label, label),
      Statement(resource, LWM.hasTimetable, timetable.uri),
      Statement(resource, LWM.hasAssignmentCount, StringLiteral(labWork.assignmentCount.toString)),
      Statement(resource, LWM.hasCourse, Resource(labWork.courseId)),
      Statement(resource, LWM.hasStartDate, DateTimeLiteral(labWork.startDate)),
      Statement(resource, LWM.hasEndDate, DateTimeLiteral(labWork.endDate)),
      Statement(resource, LWM.allowsApplications, StringLiteral("false")),
      Statement(resource, LWM.isClosed, StringLiteral("false")),
      Statement(resource, LWM.hasSemester, Resource(labWork.semester))
    ) ++ (1 to labWork.assignmentCount).map { c ⇒
        val assoc = ResourceUtils.createResource(lwmNamespace)
        Statement(assoc, RDF.typ, LWM.AssignmentAssociation) :: Statement(assoc, RDF.typ, OWL.NamedIndividual) ::
          Statement(assoc, LWM.hasLabWork, resource) :: Statement(assoc, LWM.hasOrderId, StringLiteral(s"$c")) ::
          Statement(resource, LWM.hasAssignmentAssociation, assoc) :: Nil
      }.flatten

    for (i ← 'A'.toInt until 'A'.toInt + labWork.groupCount) {
      val group = ResourceUtils.createResource(lwmNamespace)
      val groupStatements = List(
        Statement(group, RDF.typ, LWM.Group),
        Statement(group, RDF.typ, OWL.NamedIndividual),
        Statement(group, LWM.hasLabWork, resource),
        Statement(group, LWM.hasId, StringLiteral(s"${i.toChar}")),
        Statement(resource, LWM.hasGroup, group)
      )
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(groupStatements: _*))
    }

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.LabWork)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LabWork)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }
}

/**
  * An assignment group.
  * @param id id of this group
  * @param labwork id of this associated labwork
  * @param studentsURIs gmIds of the students
  */
case class LabWorkGroup(id: String, labwork: Resource, studentsURIs: List[String])

object LabworkGroups {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(group: LabWorkGroup): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Group),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, StringLiteral(group.id)),
      Statement(resource, RDFS.label, StringLiteral(s"Gruppe ${group.id}")),
      Statement(resource, LWM.hasLabWork, group.labwork),
      Statement(group.labwork, LWM.hasGroup, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(group: LabWorkGroup): Future[LabWorkGroup] = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Group, Vocabulary.LWM.hasId, StringLiteral(group.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeGroup)
    val p = Promise[LabWorkGroup]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(g ⇒ g.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(group) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Group)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Group)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(labwork ⇒ Individual(labwork.s)).toList
    }
  }

  def isLabWorkGroup(resource: Resource): Future[Boolean] = sparqlExecutionContext.executeBooleanQuery(s"ASK {$resource ${Vocabulary.RDF.typ} ${LWM.Group}}")
}
