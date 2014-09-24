package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic
import utils.semantic.Vocabulary.{ RDFS, OWL, LWM, RDF }
import utils.semantic._

import scala.concurrent.{ Promise, Future }
import scala.concurrent.ExecutionContext.Implicits.global

object Assignments {
  def create(assignment: Assignment): Future[Individual] = {
    val id = UUID.randomUUID()
    val courseResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(courseResource, RDF.typ, LWM.Assignment),
      Statement(courseResource, RDF.typ, OWL.NamedIndividual),
      Statement(courseResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(courseResource, RDFS.label, StringLiteral(assignment.id)),
      Statement(courseResource, LWM.hasText, StringLiteral(assignment.text)),
      Statement(courseResource, LWM.hasDescription, StringLiteral(assignment.description))
    ) ++ assignment.courses.map(c ⇒ Statement(courseResource, LWM.hasCourse, Resource(c))) ++ assignment.topics.map(t ⇒ Statement(courseResource, LWM.hasTopic, StringLiteral(t)))

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(courseResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Assignment)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an Assignment"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Assignment)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }
}

case class Assignment(id: String, description: String, text: String, topics: List[String], courses: List[String])

case class AssignmentFormModel(id: String, description: String, text: String, topics: String, courses: List[String])

case class AssignmentAssociation(assignment: Resource, labwork: Resource, preparationTime: Int)

case class AssignmentAssociationFormModel(assignment: String, preparationTime: Int)

object AssignmentForms {
  val assignmentForm = Form(mapping(
    "id" -> nonEmptyText,
    "description" -> nonEmptyText,
    "text" -> nonEmptyText,
    "topics" -> nonEmptyText,
    "courses" -> list(nonEmptyText)
  )(AssignmentFormModel.apply)(AssignmentFormModel.unapply))

  val assignmentAssociationForm = Form(mapping(
    "assignment" -> nonEmptyText,
    "preparationTime" -> number
  )(AssignmentAssociationFormModel.apply)(AssignmentAssociationFormModel.unapply))

  val assignmentSolutionForm = Form(mapping(
    "filename" -> nonEmptyText,
    "text" -> nonEmptyText
  )(AssignmentSolutionFormModel.apply)(AssignmentSolutionFormModel.unapply))
}

object AssignmentAssociations {
  def create(association: AssignmentAssociation): Future[Individual] = {
    val id = UUID.randomUUID()
    val associationResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(associationResource, RDF.typ, LWM.AssignmentAssociation),
      Statement(associationResource, RDF.typ, OWL.NamedIndividual),
      Statement(associationResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(associationResource, LWM.hasAssignment, association.assignment),
      Statement(associationResource, LWM.hasPreparationTime, StringLiteral(s"${association.preparationTime}")),
      Statement(associationResource, LWM.hasLabWork, association.labwork)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(associationResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.AssignmentAssociation)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an AssignmentAssociation"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.AssignmentAssociation)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }
}

case class AssignmentSolution(fileName: String, text: String, assignment: Resource)
case class AssignmentSolutionFormModel(name: String, text: String)

object AssignmentSolutions {
  def create(solution: AssignmentSolution): Future[Individual] = {
    val id = UUID.randomUUID()
    val solutionResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(solutionResource, RDF.typ, LWM.AssignmentSolution),
      Statement(solutionResource, RDF.typ, OWL.NamedIndividual),
      Statement(solutionResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(solutionResource, LWM.hasAssignment, solution.assignment),
      Statement(solution.assignment, LWM.hasSolution, solutionResource),
      Statement(solutionResource, LWM.hasFileName, StringLiteral(solution.fileName)),
      Statement(solutionResource, LWM.hasText, StringLiteral(solution.text))
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(solutionResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.AssignmentSolution)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an AssignmentSolution"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.AssignmentSolution)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(solution ⇒ Individual(solution.s)).toList
    }
  }
}