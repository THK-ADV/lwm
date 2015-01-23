package controllers

import actors.SessionHandler.Valid
import actors.{ SessionHandler, SupervisionSocketActor }
import akka.util.Timeout
import controllers.SessionManagement._
import controllers.SupervisionChangeWrites.SupervisionChange
import models.Students
import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter }
import org.joda.time.{ LocalTime, LocalDate }
import play.api.libs.json.{ Reads, JsValue }
import play.api.mvc._
import play.libs.Akka
import utils.Security.Authentication
import utils.TransactionSupport
import utils.semantic.Vocabulary.lwm
import utils.semantic.{ StringLiteral, Individual, Resource }
import play.api.Play.current

import scala.concurrent.{ Promise, Future }
import scala.util.control.NonFatal

object SupervisionChangeWrites {

  import play.api.libs.json._
  import play.api.libs.json.Reads._
  import play.api.libs.functional.syntax._

  case class SupervisionChange(association: String, attended: Boolean, passed: Boolean, ss_passed: Boolean, ss_attended: Boolean, dirty: Boolean)

  implicit val supervisionChangeReads: Reads[SupervisionChange] = (
    (JsPath \ "association").read[String] and
    (JsPath \ "attended").read[Boolean] and
    (JsPath \ "passed").read[Boolean] and
    (JsPath \ "ss_passed").read[Boolean] and
    (JsPath \ "ss_attended").read[Boolean] and
    (JsPath \ "dirty").read[Boolean]
  )(SupervisionChange.apply _)
}

object SupervisionController extends Controller with Authentication with TransactionSupport {

  import scala.concurrent.duration._
  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global
  import SupervisionChangeWrites._

  import play.api.Play.current
  override val system = Akka.system()

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

  def supervise(id: String, date: String, time: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      Future(Ok(views.html.supervision(Resource(id), LocalDate.parse(date), LocalTime.parse(time, DateTimeFormat.forPattern("HH:mm"))))).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def supervisionPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    import utils.Global._

    Action(parse.json) { implicit request ⇒

      val json = request.body

      (json \ "data").as[List[JsValue]].map(_.asOpt[SupervisionChange]).flatten.map { entry ⇒
        val i = Individual(Resource(entry.association))
        i.props.get(lwm.hasAttended).map { attendedList ⇒
          attendedList.map { attended ⇒
            i.remove(lwm.hasAttended, attended)
            modifyTransaction(session.user, i.uri, s"Attending flag removed for Student ${Students.studentForLabworkAssociation(i.uri).mkString(" ")} by ${session.user}")
          }
        }
        i.props.get(lwm.hasPassed).map { passedList ⇒
          passedList.map { passed ⇒
            i.remove(lwm.hasPassed, passed)
            modifyTransaction(session.user, i.uri, s"Passed flag removed for Student ${Students.studentForLabworkAssociation(i.uri).mkString(" ")} by ${session.user}")
          }
        }
        i.add(lwm.hasAttended, StringLiteral(entry.attended.toString))
        modifyTransaction(session.user, i.uri, s"Attending flag changed to ${entry.attended} for Student  ${Students.studentForLabworkAssociation(i.uri).mkString(" ")} by ${session.user}")
        i.add(lwm.hasPassed, StringLiteral(entry.passed.toString))
        modifyTransaction(session.user, i.uri, s"Passed flag changed to ${entry.passed} for Student ${Students.studentForLabworkAssociation(i.uri).mkString(" ")} by ${session.user}")
      }
      Ok("Updates committed")
    }
  }
}