package utils

import actors.TransactionsLoggerActor.Transaction
import akka.actor.ActorSystem
import models.{ ModifyAction, DeleteAction, CreateAction }
import org.joda.time.LocalDateTime
import utils.semantic.Resource

trait TransactionSupport {
  def system: ActorSystem
  def createTransaction(user: String, actionObject: Resource, description: String) = {
    system.eventStream.publish(Transaction(user, LocalDateTime.now(), CreateAction(actionObject, description)))
  }
  def deleteTransaction(user: String, actionObject: Resource, description: String) = {
    system.eventStream.publish(Transaction(user, LocalDateTime.now(), DeleteAction(actionObject, description)))
  }
  def modifyTransaction(user: String, actionObject: Resource, description: String) = {
    system.eventStream.publish(Transaction(user, LocalDateTime.now(), ModifyAction(actionObject, description)))
  }
}
