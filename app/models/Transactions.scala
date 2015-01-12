package models

import actors.TransactionsLoggerActor.Transaction
import com.hp.hpl.jena.query.ParameterizedSparqlString
import utils.Global._
import utils.QuerySolution
import utils.semantic.Vocabulary._
import utils.semantic._

import scala.concurrent.Future

sealed trait Action {
  def actionObject: Resource
  def description: String
}

case class CreateAction(actionObject: Resource, description: String) extends Action
case class DeleteAction(actionObject: Resource, description: String) extends Action
case class ModifyAction(actionObject: Resource, description: String) extends Action

object Transactions {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(transaction: Transaction): Future[Individual] = {
    val transactionResource = ResourceUtils.createResource(lwmNamespace)

    Actions.create(transaction.action).flatMap { action ⇒
      val statements = List(
        Statement(transactionResource, rdf.typ, lwm.Transaction),
        Statement(transactionResource, rdf.typ, owl.NamedIndividual),
        Statement(transactionResource, lwm.time, DateTimeLiteral(transaction.time)),
        Statement(transactionResource, lwm.hasActor, StringLiteral(transaction.actor)),
        Statement(transactionResource, lwm.actionObject, action.uri)
      )

      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
        Individual(transactionResource)
      }
    }

  }

  def all(): List[QuerySolution] = {
    import utils.Global._
    import utils.Implicits._
    """
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |
      |select * where {
      | ?transaction rdf:type lwm:Transaction .
      | ?transaction lwm:time ?time .
      | ?transaction lwm:hasActor ?actor .
      | ?transaction lwm:actionObject ?object .
      | ?object lwm:hasDescription ?description .
      |} order by desc(?time)
    """.stripMargin.execSelect()
  }
}

object Actions {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(action: Action): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)

    val typeStatements = action match {
      case ca: CreateAction ⇒
        Statement(resource, rdf.typ, lwm.Action) :: Statement(resource, rdf.typ, lwm.CreateAction) :: Nil
      case da: DeleteAction ⇒
        Statement(resource, rdf.typ, lwm.Action) :: Statement(resource, rdf.typ, lwm.DeleteAction) :: Nil
      case ma: ModifyAction ⇒
        Statement(resource, rdf.typ, lwm.Action) :: Statement(resource, rdf.typ, lwm.ModifyAction) :: Nil
    }

    val statements = List(
      Statement(resource, rdf.typ, owl.NamedIndividual),
      Statement(resource, lwm.hasDescription, StringLiteral(action.description)),
      Statement(resource, lwm.actionObject, action.actionObject)
    )

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements ::: typeStatements: _*)).map { r ⇒
      Individual(resource)
    }
  }
}