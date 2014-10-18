package actors

import java.net.URLDecoder

import akka.actor.{ Props, ActorRef, Actor }
import akka.actor.Actor.Receive
import play.api.libs.json.{ JsString, JsObject, JsValue }
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Individual, Resource }

object SupervisionSocketActor {
  def props(out: ActorRef) = Props(new SupervisionSocketActor(out))
}

class SupervisionSocketActor(out: ActorRef) extends Actor {
  import utils.Global._

  override def receive: Receive = {
    case s: JsValue ⇒
      println(s"WebSocket received: $s")
      val maybeType = (s \ "type").asOpt[String]
      val maybeAssociation = (s \ "association").asOpt[String]
      for {
        t ← maybeType
        a ← maybeAssociation
      } {
        val res = Individual(Resource(a))
        t.toLowerCase match {
          case "passed-change" ⇒
            handlePassed(res)
          case "attendance-change" ⇒
            handleAttendance(res)
        }
      }
  }

  def handlePassed(association: Individual) = {
    association.props.get(LWM.hasPassed) match {
      case None ⇒
        association.add(LWM.hasPassed, StringLiteral("true"))
      case Some(passedList) ⇒
        if (passedList.size > 1) {
          passedList.map { passed ⇒
            association.remove(LWM.hasPassed, passed)
          }
          association.add(LWM.hasPassed, StringLiteral("false"))
        } else if (passedList.size == 0) {
          association.add(LWM.hasPassed, StringLiteral("true"))
        } else {
          val passed = passedList.head.toString.toBoolean
          association.update(LWM.hasPassed, StringLiteral(passed.toString.toLowerCase), StringLiteral((!passed).toString.toLowerCase))
        }
    }
  }

  def handleAttendance(association: Individual) = {
    association.props.get(LWM.hasAttended) match {
      case None ⇒
        association.add(LWM.hasAttended, StringLiteral("true"))
      case Some(attendingList) ⇒
        if (attendingList.size > 1) {
          attendingList.map { attends ⇒
            association.remove(LWM.hasAttended, attends)
          }
        } else if (attendingList.size == 0) {
          association.add(LWM.hasAttended, StringLiteral("true"))
        } else {
          val attended = attendingList.head.toString.toBoolean
          association.update(LWM.hasAttended, StringLiteral(attended.toString.toLowerCase), StringLiteral((!attended).toString.toLowerCase))
        }
    }
  }

}
