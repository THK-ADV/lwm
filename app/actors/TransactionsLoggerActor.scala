package actors

import actors.TransactionsLoggerActor.Transaction
import akka.actor.Actor
import akka.actor.Actor.Receive
import models.Action
import org.joda.time.LocalDateTime
import utils.semantic.Resource

object TransactionsLoggerActor {
  case class Transaction(actor: Resource, time: LocalDateTime, action: Action)
}

class TransactionsLoggerActor extends Actor {
  override def receive: Receive = {
    case Transaction(actor, time, action) ⇒
  }
}
