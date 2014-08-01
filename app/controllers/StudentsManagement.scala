package controllers

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import controllers.SessionHandler.{Invalid, Valid}
import models.{Students, UserForms}
import play.api.mvc.{Action, Controller, Security}
import play.libs.Akka

import scala.concurrent.Future

object StudentWebSocketActor {
  def props(out: ActorRef) = Props(new StudentWebSocketActor(out))
}

class StudentWebSocketActor(out: ActorRef) extends Actor {
  override def receive: Receive = ???
}


object StudentsManagement extends Controller {

  import akka.pattern.ask

import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")


  def index() = Action.async { request =>
    request.session.get("session") match {
      case None => Future.successful(Redirect(routes.Application.index()).withNewSession)
      case Some(id) =>
        val responseFut = (sessionsHandler ? SessionHandler.SessionValidationRequest(id)).mapTo[SessionHandler.ValidationResponse]
        for {
          response <- responseFut
          students <- Students.all()
        } yield {
          response match {
            case Valid => Ok(views.html.students(students.toList, UserForms.studentForm))
            case Invalid => Redirect(routes.Application.index()).withNewSession
          }
        }
    }


  }


  // TODO Websocket Actor
  //  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
  //    StudentWebSocketActor.props(out)
  //  }

  def studentPost = Action.async { implicit request =>
    UserForms.studentForm.bindFromRequest.fold(
      formWithErrors => {
        for (all <- Students.all()) yield {
          BadRequest(views.html.students(all.toList, formWithErrors))
        }

      },
      student => {
        request.session.get("session") match {
          case None => Future.successful(Redirect(routes.Application.index()).withNewSession)
          case Some(id) =>
            val responseFut = (sessionsHandler ? SessionHandler.SessionRequest(id)).mapTo[SessionHandler.Session]
            for {
              response <- responseFut
            } yield {
              if (response.user.equalsIgnoreCase(request.session.get(Security.username).get)) {
                request.session.get("setup") match {
                  case Some("1") =>
                    Students.create(student)
                    Redirect(routes.StudentDashboardController.dashboard())
                  case Some("0") =>
                    Redirect(routes.StudentDashboardController.dashboard())
                  case _ =>
                    Redirect(routes.StudentDashboardController.dashboard())
                }
              } else {
                if (response.role.contains(Permissions.UserModification)) {
                  Students.create(student)
                  Redirect(routes.StudentsManagement.index())
                } else {
                  Redirect(routes.Application.index()).withNewSession
                }
              }
            }
        }

      }
    )
  }
}
