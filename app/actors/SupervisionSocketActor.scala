package actors

import java.net.URLDecoder

import actors.SupervisionSocketActor.SupervisionChange
import akka.actor.{ ActorSystem, Props, ActorRef, Actor }
import akka.actor.Actor.Receive
import models.Students
import play.api.libs.json.{ JsBoolean, JsString, JsObject, JsValue }
import utils.TransactionSupport
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Individual, Resource }

object SupervisionSocketActor {
  case class SupervisionChange(change: JsValue, ref: ActorRef)
  def props(out: ActorRef) = Props(new SupervisionSocketActor(out))
}

class SupervisionSocketActor(out: ActorRef) extends Actor with TransactionSupport {
  import utils.Global._
  import scala.concurrent.duration._
  import context.dispatcher
  override val system: ActorSystem = context.system

  context.system.eventStream.subscribe(self, classOf[SupervisionChange])
  context.system.scheduler.schedule(1.second, 1.second, out, JsObject(Seq(
    "type" -> JsString("ping")
  )))

  override def receive: Receive = {
    case s: JsValue ⇒
      val maybeType = (s \ "type").asOpt[String]
      val maybeAssociation = (s \ "association").asOpt[String]
      val maybeUser = (s \ "user").asOpt[String]
      for {
        t ← maybeType
        a ← maybeAssociation
        u ← maybeUser
      } {
        val res = Individual(Resource(a))
        t.toLowerCase match {
          case "passed-change" ⇒
            val status = handlePassed(res)
            modifyTransaction(u, res.uri, s"Passed flag changed to $status for ${Students.studentForLabworkAssociation(res.uri).mkString(" ")} by $u")
            context.system.eventStream.publish(SupervisionChange(JsObject(Seq(
              "type" -> JsString("passed"),
              "association" -> JsString(a),
              "user" -> JsString(u),
              "id" -> JsString(s"#passed-${a.hashCode}"),
              "status" -> JsBoolean(status)
            )), self))
          case "attendance-change" ⇒
            val status = handleAttendance(res)
            modifyTransaction(u, res.uri, s"Attending flag changed to $status for ${Students.studentForLabworkAssociation(res.uri).mkString(" ")} by $u")
            context.system.eventStream.publish(SupervisionChange(JsObject(Seq(
              "type" -> JsString("attending"),
              "association" -> JsString(a),
              "user" -> JsString(u),
              "id" -> JsString(s"#attendance-${a.hashCode}"),
              "status" -> JsBoolean(status)
            )), self))
        }
      }
    case SupervisionChange(change, ref) if ref != self ⇒
      out ! change

  }

  def handlePassed(association: Individual): Boolean = {

    association.props.get(LWM.hasPassed) match {
      case None ⇒
        association.add(LWM.hasPassed, StringLiteral("true"))
        true
      case Some(passedList) ⇒
        if (passedList.size > 1) {
          passedList.map { passed ⇒
            association.remove(LWM.hasPassed, passed)
          }
          association.add(LWM.hasPassed, StringLiteral("false"))
          false
        } else if (passedList.size == 0) {
          association.add(LWM.hasPassed, StringLiteral("true"))
          true
        } else {
          val passed = passedList.head.toString.toBoolean
          association.update(LWM.hasPassed, StringLiteral(passed.toString.toLowerCase), StringLiteral((!passed).toString.toLowerCase))
          !passed
        }
    }
  }

  def handleAttendance(association: Individual): Boolean = {
    association.props.get(LWM.hasAttended) match {
      case None ⇒
        association.add(LWM.hasAttended, StringLiteral("true"))
        true
      case Some(attendingList) ⇒
        if (attendingList.size > 1) {
          attendingList.map { attends ⇒
            association.remove(LWM.hasAttended, attends)
          }
          association.add(LWM.hasAttended, StringLiteral("false"))
          false
        } else if (attendingList.size == 0) {
          association.add(LWM.hasAttended, StringLiteral("true"))
          true
        } else {
          val attended = attendingList.head.toString.toBoolean
          association.update(LWM.hasAttended, StringLiteral(attended.toString.toLowerCase), StringLiteral((!attended).toString.toLowerCase))
          !attended
        }
    }
  }

}
