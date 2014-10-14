package models

import java.util.Date

import org.joda.time.{ LocalDate, DateTime }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class LabWork(course: Resource, semester: Resource)
case class LabWorkFormModel(courseId: String, semester: String)
case class LabworkUpdateModel(courseId: String, semester: String, startDate: Date, endDate: Date)

// TODO course id should be courseResourceURI

object LabWorkForms {

  import play.api.data.Forms._
  import play.api.data._

  val labworkForm = Form(
    mapping(
      "courseId" -> nonEmptyText,
      "semester" -> nonEmptyText
    )(LabWorkFormModel.apply)(LabWorkFormModel.unapply)
  )

  val labworkUpdateForm = Form(
    mapping(
      "courseId" -> nonEmptyText,
      "semester" -> nonEmptyText,
      "startDate" -> date,
      "endDate" -> date
    )(LabworkUpdateModel.apply)(LabworkUpdateModel.unapply)
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
    val semesterIndividual = Individual(labWork.semester)
    val startDate = semesterIndividual.props.getOrElse(LWM.hasStartDate, List(new DateLiteral(LocalDate.now()))).head
    val endDate = semesterIndividual.props.getOrElse(LWM.hasEndDate, List(new DateLiteral(LocalDate.now()))).head

    val courseIndividual = Individual(labWork.course)

    val resource = ResourceUtils.createResource(lwmNamespace)
    val timetable = Timetables.create(Timetable(resource))
    val labworkApplicationList = LabworkApplicationLists.create(LabworkApplicationList(resource))

    val label = courseIndividual.props.getOrElse(RDFS.label, List(StringLiteral(""))).head.asLiteral().get

    val statements = List(
      Statement(resource, RDF.typ, LWM.LabWork),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, RDFS.label, label),
      Statement(resource, LWM.hasTimetable, timetable.uri),
      Statement(resource, LWM.hasCourse, labWork.course),
      Statement(resource, LWM.hasStartDate, startDate),
      Statement(resource, LWM.hasEndDate, endDate),
      Statement(resource, LWM.allowsApplications, StringLiteral("false")),
      Statement(resource, LWM.isClosed, StringLiteral("false")),
      Statement(resource, LWM.hasSemester, labWork.semester)
    )

    labworkApplicationList.flatMap { list ⇒
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
        Individual(resource)
      }
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
  */
case class LabWorkGroup(id: String, labwork: Resource)

object LabworkGroups {

  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(group: LabWorkGroup): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Group),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasGroupId, StringLiteral(group.id)),
      Statement(resource, RDFS.label, StringLiteral(group.id)),
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
