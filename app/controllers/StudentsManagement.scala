package controllers

import akka.actor.Actor.Receive
import akka.actor.{Props, ActorRef, Actor}
import akka.util.Timeout
import models.{Student, Students}
import play.api.Play.current
import play.api.data.Forms._
import play.api.data._
import play.api.mvc.{Action, Controller, WebSocket}
import play.libs.Akka

import scala.concurrent.Future

object StudentWebSocketActor {
  def props(out: ActorRef) = Props(new StudentWebSocketActor(out))
}

class StudentWebSocketActor(out: ActorRef) extends Actor {
  override def receive: Receive = ???
}


object StudentsManagement extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

  val studentForm = Form(
    mapping(
      "id" -> text,
      "firstname" -> text,
      "lastname" -> text,
      "registrationNumber" -> text,
      "email" -> email,
      "phone" -> text,
      "degree" -> text
    )(Student.apply)(Student.unapply)
  )

  def index() = Action.async {
    for (all <- Students.all()) yield {
      Ok(views.html.students(all.toList, studentForm))
    }
  }



  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
    StudentWebSocketActor.props(out)
  }

  def studentPost = Action.async { implicit request =>
    studentForm.bindFromRequest.fold(
      formWithErrors => {
        for (all <- Students.all()) yield {
          BadRequest(views.html.students(all.toList, formWithErrors))
        }

      },
      student => {
        for (all <- Students.all()) yield {
          Students.create(student)
          Redirect(routes.StudentsManagement.index())
        }
      }
    )
  }
}
