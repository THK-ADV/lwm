package models

import java.util.UUID

import actors.TransactionsLoggerActor.Transaction
import com.hp.hpl.jena.query.ParameterizedSparqlString

import utils.{UpdateHost, QueryHost, QuerySolution}
import utils.semantic.Vocabulary._
import utils.semantic._

import scala.concurrent.{Promise, Future}

sealed trait Action {
  def actionObject: Resource
  def description: String
}

case class CreateAction(actionObject: Resource, description: String) extends Action
case class DeleteAction(actionObject: Resource, description: String) extends Action
case class ModifyAction(actionObject: Resource, description: String) extends Action

object Transactions {

  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.Implicits._

  def size()(implicit queryHost: QueryHost): Int = {
    import utils.Implicits._
    """
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |
      |select (count(distinct ?transaction) as ?count) {
      |   ?transaction rdf:type lwm:Transaction
      |}
    """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }

    def create(transaction: Transaction)(implicit updateHost: UpdateHost) = {
      import utils.Global.lwmNamespace
      val resource = Resource(s"${lwmNamespace}transactions/${UUID.randomUUID()}")

    }

  def create(transaction: Transaction): Future[Individual] = {
    import utils.Global._
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

  def create(action: Action)(implicit updateHost: UpdateHost): Future[Resource] = {
    import utils.Global.lwmNamespace
    import utils.Implicits._
    import scala.concurrent.blocking

    val resource = Resource(s"${lwmNamespace}actions/${UUID.randomUUID()}")

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

    val p = Promise[Resource]()

    blocking {
      SPARQLBuilder.insertStatements(statements ::: typeStatements: _*).execUpdate()
    }
    
    p.future
  }
}