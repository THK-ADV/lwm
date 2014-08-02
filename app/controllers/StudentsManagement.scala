package controllers

import actors.SessionHandler
import actors.SessionHandler.{Invalid, Valid}
import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import controllers.SessionManagement._
import controllers.UserManagement._
import models.{Users, Students, UserForms}
import play.api.mvc.{Action, Controller, Security}
import play.libs.Akka
import utils.Security.Authentication

import scala.concurrent.Future

object StudentWebSocketActor {
  def props(out: ActorRef) = Props(new StudentWebSocketActor(out))
}

class StudentWebSocketActor(out: ActorRef) extends Actor {
  override def receive: Receive = ???
}


object StudentsManagement extends Controller with Authentication {

  import akka.pattern.ask

import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")


  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async { request =>
      for{
        students <- Students.all()
      } yield{
        Ok(views.html.students(students.toList, UserForms.studentForm))
      }
    }
  }


  // TODO Websocket Actor
  //  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
  //    StudentWebSocketActor.props(out)
  //  }

  def studentFirstTimeSelf = hasSession { session =>
    Action { implicit request =>
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors => {
          BadRequest(views.html.firstTimeInputStudents(formWithErrors))
        },
        student => {
          Students.create(student)
          Redirect(routes.StudentDashboardController.dashboard())
        }
      )
    }
  }


  def studentPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*){session =>
    Action.async { implicit request =>
      UserForms.studentForm.bindFromRequest.fold(
        formWithErrors => {
          for (all <- Students.all()) yield {
            BadRequest(views.html.students(all.toList, formWithErrors))
          }
        },
        student => {
          Students.create(student)
          Future.successful(Redirect(routes.StudentsManagement.index()))
        }
      )
    }
  }
}
