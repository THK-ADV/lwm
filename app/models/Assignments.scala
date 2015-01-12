package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms._
import utils.Global._
import utils.semantic.Vocabulary.{ lwm, owl, rdf, rdfs }
import utils.semantic._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }

object LiveAssignments {

  def create(liveAssignment: LiveAssignment): Future[Individual] = {
    val id = UUID.randomUUID()
    val courseResource = ResourceUtils.createResource(lwmNamespace, id)
    val statements = List(
      Statement(courseResource, rdf.typ, lwm.LiveAssignment),
      Statement(courseResource, rdf.typ, owl.NamedIndividual),
      Statement(courseResource, lwm.hasId, StringLiteral(id.toString)),
      Statement(courseResource, rdfs.label, StringLiteral(liveAssignment.title)),
      Statement(courseResource, lwm.hasText, StringLiteral(liveAssignment.assignment)),
      Statement(courseResource, lwm.hasHints, StringLiteral(liveAssignment.example))
    )

    val topicStatements = liveAssignment.topics.map(t ⇒ Statement(courseResource, lwm.hasTopic, StringLiteral(t.toLowerCase.trim)))

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(topicStatements ::: statements: _*)).map(b ⇒ Individual(courseResource))
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.LiveAssignment)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def all(tag: String): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.LiveAssignment, lwm.hasTopic, StringLiteral(tag))).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(course ⇒ Individual(course.s)).toList
    }
  }

  def tags(): Future[List[String]] = Future {
    import utils.Implicits._

    s"""
         |select distinct ?topic where {
         | ?live ${rdf.typ} ${lwm.LiveAssignment} .
         | ?live ${lwm.hasTopic} ?topic
         |} order by asc(?topic)
       """.stripMargin.execSelect().map { solution ⇒
      solution.data.get("topic").map(_.asLiteral().getString)
    }.flatten
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.LiveAssignment)) {
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
      Statement(courseResource, rdf.typ, lwm.Assignment),
      Statement(courseResource, rdf.typ, owl.NamedIndividual),
      Statement(courseResource, lwm.hasId, StringLiteral(id.toString)),
      Statement(courseResource, rdfs.label, StringLiteral(assignment.heading)),
      Statement(courseResource, lwm.hasText, StringLiteral(assignment.text)),
      Statement(courseResource, lwm.hasHints, StringLiteral(assignment.hints)),
      Statement(courseResource, lwm.hasLearningGoals, StringLiteral(assignment.goals)),
      Statement(courseResource, lwm.hasDescription, StringLiteral(assignment.description))
    ) ++ assignment.courses.map(c ⇒ Statement(courseResource, lwm.hasCourse, Resource(c))) ++ assignment.topics.map(t ⇒ Statement(courseResource, lwm.hasTopic, StringLiteral(t)))

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(courseResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.Assignment)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an Assignment"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Assignment)).map { stringResult ⇒
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
      Statement(associationResource, rdf.typ, lwm.AssignmentAssociation),
      Statement(associationResource, rdf.typ, owl.NamedIndividual),
      Statement(associationResource, lwm.hasId, StringLiteral(id.toString)),
      Statement(associationResource, lwm.hasOrderId, StringLiteral(s"${association.orderId}")),
      Statement(association.labwork, lwm.hasAssignmentAssociation, associationResource),
      Statement(associationResource, lwm.hasLabWork, association.labwork),
      Statement(associationResource, lwm.allowsApplications, StringLiteral("false"))
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(associationResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.AssignmentAssociation)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an AssignmentAssociation"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.AssignmentAssociation)).map { stringResult ⇒
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
      Statement(solutionResource, rdf.typ, lwm.AssignmentSolution),
      Statement(solutionResource, rdf.typ, owl.NamedIndividual),
      Statement(solutionResource, lwm.hasId, StringLiteral(id.toString)),
      Statement(solutionResource, lwm.hasAssignment, solution.assignment),
      Statement(solution.assignment, lwm.hasSolution, solutionResource),
      Statement(solutionResource, lwm.hasFileName, StringLiteral(solution.fileName)),
      Statement(solutionResource, lwm.hasText, StringLiteral(solution.text))
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(b ⇒ Individual(solutionResource))
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.AssignmentSolution)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not an AssignmentSolution"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.AssignmentSolution)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(solution ⇒ Individual(solution.s)).toList
    }
  }
}
