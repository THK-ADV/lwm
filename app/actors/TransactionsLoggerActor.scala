package actors

import actors.TransactionsLoggerActor.Transaction
import akka.actor.{ Props, ActorLogging, Actor }
import akka.actor.Actor.Receive
import models.{ Transactions, Action }
import org.joda.time.LocalDateTime
import utils.semantic.Resource

object TransactionsLoggerActor {
  case class Transaction(actor: String, time: LocalDateTime, action: Action)
  def props() = Props(new TransactionsLoggerActor)
}

class TransactionsLoggerActor extends Actor with ActorLogging {
  import scala.concurrent.ExecutionContext.Implicits.global

  context.system.eventStream.subscribe(self, classOf[Transaction])

  override def receive: Receive = {
    case t @ Transaction(actor, time, action) ⇒
      Transactions.create(t)
  }
}
