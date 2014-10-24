package actors

import actors.SessionHandler.{ OnlineUserCountChange, OnlineStudentCountChange }
import akka.actor.Actor.Receive
import akka.actor.{ Props, Actor, ActorRef }
import play.api.libs.json.{ JsNumber, JsString, JsObject, JsValue }
import play.libs.Akka

object UserCountSocketActor {

  def props(out: ActorRef) = Props(new UserCountSocketActor(out))
}

class UserCountSocketActor(out: ActorRef) extends Actor {
  import akka.pattern.ask
  import scala.concurrent.duration._
  import akka.util.Timeout

  import context.dispatcher
  implicit val timeout = Timeout(5.seconds)

  val sessionsHandler = Akka.system.actorSelection("user/sessions")

  context.system.eventStream.subscribe(self, classOf[OnlineStudentCountChange])
  context.system.eventStream.subscribe(self, classOf[OnlineUserCountChange])

  override def preStart(): Unit = {
    (sessionsHandler ? SessionHandler.OnlineUserCountRequest).map {
      case users @ OnlineUserCountChange(count) ⇒
        self ! users
    }

    (sessionsHandler ? SessionHandler.OnlineStudentCountRequest).map {
      case students @ OnlineStudentCountChange(count) ⇒
        self ! students
    }
  }

  override def receive: Receive = {
    case OnlineStudentCountChange(count) ⇒
      out ! JsObject(Seq(
        "type" -> JsString("studentCount"),
        "count" -> JsNumber(count)
      ))
    case OnlineUserCountChange(count) ⇒
      out ! JsObject(Seq(
        "type" -> JsString("userCount"),
        "count" -> JsNumber(count)
      ))

  }
}
