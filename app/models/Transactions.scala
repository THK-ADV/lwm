package models

import actors.TransactionsLoggerActor.Transaction
import utils.Global._
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
        Statement(transactionResource, RDF.typ, LWM.Transaction),
        Statement(transactionResource, RDF.typ, OWL.NamedIndividual),
        Statement(transactionResource, LWM.time, DateTimeLiteral(transaction.time)),
        Statement(transactionResource, LWM.hasActor, transaction.actor),
        Statement(transactionResource, LWM.actionObject, action.uri)
      )

      sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
        Individual(transactionResource)
      }
    }

  }
  def all(): Future[List[Resource]] = ???
}

object Actions {
  def create(action: Action): Future[Individual] = ???
}