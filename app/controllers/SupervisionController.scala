package controllers

import actors.SessionHandler.Valid
import actors.{ SessionHandler, SupervisionSocketActor }
import akka.util.Timeout
import controllers.SupervisionChangeWrites.SupervisionChange
import org.joda.time.LocalDate
import play.api.libs.json.{ Reads, JsValue }
import play.api.mvc._
import play.libs.Akka
import utils.Security.Authentication
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Individual, Resource }
import play.api.Play.current

import scala.concurrent.{ Promise, Future }

object SupervisionChangeWrites {

  import play.api.libs.json._
  import play.api.libs.json.Reads._
  import play.api.libs.functional.syntax._

  case class SupervisionChange(association: String, attended: Boolean, passed: Boolean)

  implicit val supervisionChangeReads: Reads[SupervisionChange] = (
    (JsPath \ "association").read[String] and
    (JsPath \ "attended").read[Boolean] and
    (JsPath \ "passed").read[Boolean]
  )(SupervisionChange.apply _)
}

object SupervisionController extends Controller with Authentication {

  import scala.concurrent.duration._
  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global
  import SupervisionChangeWrites._

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

  def superviseSocket = WebSocket.tryAcceptWithActor[JsValue, JsValue] { request ⇒
    val p = Promise[Either[Result, WebSocket.HandlerProps]]()
    request.session.get("session").map { token ⇒
      (sessionsHandler ? SessionHandler.SessionValidationRequest(token)).map {
        case SessionHandler.Valid(session) ⇒
          if (session.role.contains(Permissions.ScheduleAssociationModification)) {
            p.success(Right(SupervisionSocketActor.props))
          } else {
            p.success(Left(Unauthorized("Insufficient Access Rights")))
          }
        case SessionHandler.Invalid ⇒
          p.success(Left(Unauthorized("Invalid Session")))
      }
    }

    p.future
  }

  def supervise(id: String, date: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      Future.successful(Ok(views.html.supervision(Resource(id), LocalDate.parse(date), session)))
    }
  }

  def supervisionPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    import utils.Global._

    Action.async(parse.json) { implicit request ⇒

      val json = request.body
      (json \ "data").as[List[JsValue]].map(_.asOpt[SupervisionChange]).flatten.map { entry ⇒
        val i = Individual(Resource(entry.association))
        i.props.get(LWM.hasAttended).map { attendedList ⇒
          attendedList.map { attended ⇒
            i.remove(LWM.hasAttended, attended)
          }
        }
        i.props.get(LWM.hasPassed).map { passedList ⇒
          passedList.map { passed ⇒
            i.remove(LWM.hasPassed, passed)
          }
        }
        i.add(LWM.hasAttended, StringLiteral(entry.attended.toString))
        i.add(LWM.hasPassed, StringLiteral(entry.passed.toString))
      }
      Future.successful(Ok("Updates committed"))
    }
  }
}