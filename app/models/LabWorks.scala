package models

import play.api.data.Form
import play.api.data.Forms._
import utils.semantic._

import scala.concurrent.Future

case class LabWork(id: String, name: String, groupCount: Int, assignmentCount: Int, courseId: String, degreeId: String, semester: String)

case class LabWorkApplication(courseID: String, gmID: String)


// TODO course id should be courseResourceURI


object LabWorkForms{
  val labWorkApplicationForm = Form(
    mapping(
      "courseID" -> nonEmptyText,
      "gmID" -> nonEmptyText
    )(LabWorkApplication.apply)(LabWorkApplication.unapply)
  )

  val labworkForm = Form(
    mapping(
      "id" -> nonEmptyText,
      "name" -> nonEmptyText,
      "groupCount" -> number(min=1),
      "assignmentCount" -> number(min=1),
      "courseId" -> nonEmptyText,
      "degreeId" -> nonEmptyText,
      "semester" -> nonEmptyText
    )(LabWork.apply)(LabWork.unapply)
  )
}

/**
 * Praktika
 */
object LabWorks {
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(labWork: LabWork): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.LabWork),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(labWork.id)),
      Statement(resource, LWM.hasName, Literal(labWork.name)),
      Statement(resource, RDFS.label, Literal(labWork.name)),

      Statement(resource, LWM.hasAssignmentCount, Literal(labWork.assignmentCount.toString)),
      Statement(resource, LWM.hasCourse, Resource(labWork.courseId)),
      Statement(resource, LWM.hasDegree, Resource(labWork.degreeId)),
      Statement(resource, LWM.hasSemester, Literal(labWork.semester))
    )

    for(i <- 'A'.toInt until 'A'.toInt + labWork.groupCount) {
      val group = ResourceUtils.createResource(lwmNamespace)
      val groupStatements = List(
        Statement(group, RDF.typ, LWM.Group),
        Statement(group, RDF.typ, OWL.NamedIndividual),
        Statement(group, LWM.hasLabWork, resource),
        Statement(group, LWM.hasId, Literal(s"${i.toChar}")),
        Statement(resource, LWM.hasGroup, group)
      )
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph,  groupStatements: _*))
    }
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph,  statements: _*))

    Individual(resource)
  }

  def delete(labwork: LabWork) = {
    val maybeLabwork = SPARQLBuilder.listIndividualsWithProperty(Vocabulary.LWM.hasId, Literal(labwork.id))
    val degreeResource = SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(maybeLabwork)).map(labwork => labwork.s)
    degreeResource.map(res => sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(res, lwmGraph)))
  }

  def delete(resource: Resource): Unit = {
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.LabWork)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph))
    }
  }

  def get(resource: Resource): Future[Option[LabWork]] = ???

  def all(): Future[Seq[Individual]] = Future{
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LabWork))).map(labwork => Individual(labwork.s))
  }
}


/**
 * An assignment group.
 * @param id id of this group
 * @param labwork id of this associated labwork
 * @param studentsURIs gmIds of the students
 */
case class LabWorkGroup(id: String, labwork: Resource, studentsURIs: List[String])

object LabworkGroups{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(group: LabWorkGroup): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Group),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(group.id)),
      Statement(resource, RDFS.label, Literal(s"Gruppe ${group.id}")),
      Statement(resource, LWM.hasLabWork, group.labwork),
      Statement(group.labwork, LWM.hasGroup, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph,  statements: _*))

    Individual(resource)
  }

  def delete(group: LabWorkGroup) = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(Vocabulary.LWM.Group, Vocabulary.LWM.hasId, Literal(group.id))
    val groupResources = SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(maybeGroup)).map(labwork => labwork.s)
    groupResources.map(res => sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(res, lwmGraph)))
  }
  def delete(id: String) = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(Vocabulary.LWM.Group, Vocabulary.LWM.hasId, Literal(id))
    val groupResources = SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(maybeGroup)).map(labGroup => labGroup.s)
    groupResources.map(res => sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(res, lwmGraph)))
  }
  def all(): Future[Seq[Individual]] = Future{

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Group))).map(group => Individual(group.s))

  }
}
