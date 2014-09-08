package models

import play.api.data.Form
import play.api.data.Forms._
import utils.semantic._

import scala.concurrent.{Promise, Future}

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

  def create(labWork: LabWork): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val timetable = Timetables.create(Timetable(resource))
    val statements = List(
      Statement(resource, RDF.typ, LWM.LabWork),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(labWork.id)),
      Statement(resource, LWM.hasName, Literal(labWork.name)),
      Statement(resource, RDFS.label, Literal(labWork.name)),
      Statement(resource, LWM.hasTimetable, timetable.uri),
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


    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph,  statements: _*)).map{r =>
      Individual(resource)
    }    
  }

  def delete(labwork: LabWork): Future[LabWork] = {
    val maybeLabwork = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.LabWork, Vocabulary.LWM.hasId, Literal(labwork.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeLabwork)
    val p = Promise[LabWork]()
    resultFuture.map{result =>
      val resources = SPARQLTools.statementsFromString(result).map(degree => degree.s)
      resources.map{resource =>
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{_ => p.success(labwork)}
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] =  {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.LabWork)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{b => p.success(resource)}
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LabWork)).map{stringResult =>
      SPARQLTools.statementsFromString(stringResult).map(labwork => Individual(labwork.s)).toList
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

object LabworkGroups{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(group: LabWorkGroup): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.Group),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasId, Literal(group.id)),
      Statement(resource, RDFS.label, Literal(s"Gruppe ${group.id}")),
      Statement(resource, LWM.hasLabWork, group.labwork),
      Statement(group.labwork, LWM.hasGroup, resource)
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph,  statements: _*)).map{r =>
      Individual(resource)
    }
  }


  def delete(group: LabWorkGroup): Future[LabWorkGroup] = {
    val maybeGroup = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Group, Vocabulary.LWM.hasId, Literal(group.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeGroup)
    val p = Promise[LabWorkGroup]()
    resultFuture.map{result =>
      val resources = SPARQLTools.statementsFromString(result).map(g => g.s)
      resources.map{resource =>
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{_ => p.success(group)}
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] =  {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if(individual.props(RDF.typ).contains(LWM.Group)){
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource, lwmGraph)).map{b => p.success(resource)}
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Group)).map{stringResult =>
      SPARQLTools.statementsFromString(stringResult).map(labwork => Individual(labwork.s)).toList
    }
  }
}
