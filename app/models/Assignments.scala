package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ LWM, OWL, RDF, RDFS }
import utils.semantic._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }

object LiveAssignments {

  def create(liveAssignment: LiveAssignment): Future[Individual] = {
    val id = UUID.randomUUID()
    val courseResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(courseResource, RDF.typ, LWM.LiveAssignment),
      Statement(courseResource, RDF.typ, OWL.NamedIndividual),
      Statement(courseResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(courseResource, RDFS.label, StringLiteral(liveAssignment.title)),
      Statement(courseResource, LWM.hasText, StringLiteral(liveAssignment.assignment)),
      Statement(courseResource, LWM.hasHints, StringLiteral(liveAssignment.example))
    )

    val topicStatements = liveAssignment.topics.map(t ⇒ Statement(courseResource, LWM.hasTopic, StringLiteral(t)))

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(topicStatements ::: statements: _*)).map(b ⇒ Individual(courseResource))
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.LiveAssignment)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.LiveAssignment)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Live Assignment!"))
    }
    p.future
  }

  object Forms {
    val addForm = Form(mapping(
      "title" -> text,
      "assignment" -> text,
      "example" -> text,
      "topics" -> text
    )(LiveAssignmentFormModel.apply)(LiveAssignmentFormModel.unapply))
  }
}
case class LiveAssignment(title: String, assignment: String, example: String, topics: List[String])
case class LiveAssignmentFormModel(title: String, assignment: String, example: String, topics: String)

object Assignments {
  def create(assignment: Assignment): Future[Individual] = {
    val id = UUID.randomUUID()
    val courseResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(courseResource, RDF.typ, LWM.Assignment),
      Statement(courseResource, RDF.typ, OWL.NamedIndividual),
      Statement(courseResource, LWM.hasId, StringLiteral(id.toString)),
      Statement(courseResource, RDFS.label, StringLiteral(assignment.heading)),
      Statement(courseResource, LWM.hasText, StringLiteral(assignment.text)),
      Statement(courseResource, LWM.hasHints, StringLiteral(assignment.hints)),
      Statement(courseResource, LWM.hasLearningGoals, StringLiteral(assignment.goals)),
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

case class Assignment(heading: String, description: String, text: String, goals: String, hints: String, topics: List[String], courses: List[String])

case class AssignmentFormModel(heading: String, description: String, text: String, goals: String, hints: String, topics: String, courses: List[String])

case class AssignmentAssociation(labwork: Resource, orderId: Int)

case class AssignmentAssociationFormModel(assignment: String, preparationTime: Int)

object AssignmentForms {
  val assignmentForm = Form(mapping(
    "heading" -> text,
    "description" -> text,
    "text" -> text,
    "goals" -> text,
    "hints" -> text,
    "topics" -> text,
    "courses" -> list(text)
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
      Statement(associationResource, LWM.hasOrderId, StringLiteral(s"${association.orderId}")),
      Statement(association.labwork, LWM.hasAssignmentAssociation, associationResource),
      Statement(associationResource, LWM.hasLabWork, association.labwork),
      Statement(associationResource, LWM.allowsApplications, StringLiteral("false"))
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
