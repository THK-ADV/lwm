package controllers

import actors.SessionHandler
import controllers.UserManagement._
import models._
import play.api.mvc.{ Action, Controller }
import play.libs.Akka
import utils.Security.Authentication
import utils.semantic.Individual

import scala.concurrent.Future
import scala.util.control.NonFatal

object FirstTimeSetupController extends Controller with Authentication {

  import akka.pattern.ask
  import akka.util.Timeout
  import scala.concurrent.duration._
  import utils.Global._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val timeout = Timeout(5.seconds)
  val sessionsHandler = Akka.system.actorSelection("user/sessions")

  def setupStudent() = hasSession { session ⇒
    Action.async { implicit request ⇒
      (for {
        name ← (sessionsHandler ? SessionHandler.NameRequest(session.user)).mapTo[(String, String)]
        degreeResources ← Degrees.all()
        degrees = degreeResources.map(d ⇒ Individual(d))
      } yield {
        val filledForm = UserForms.studentForm.fill(Student(session.user, name._1, name._2, "", s"${session.user}@gm.fh-koeln.de", "", ""))
        Ok(views.html.firstTimeInputStudents(degrees, filledForm))
      }).recoverWith {
        case NonFatal(t) ⇒
          for {
            degreeResources ← Degrees.all()
            degrees = degreeResources.map(d ⇒ Individual(d))
          } yield {
            val filledForm = UserForms.studentForm.fill(Student(session.user, "", "", "", s"${session.user}@gm.fh-koeln.de", "", ""))
            Ok(views.html.firstTimeInputStudents(degrees, filledForm))
          }
      }
    }
  }

  def setupUser() = hasSession { session ⇒
    Action.async { implicit request ⇒
      (for (name ← (sessionsHandler ? SessionHandler.NameRequest(session.user)).mapTo[(String, String)]) yield {
        val filledForm = UserForms.userForm.fill(User(session.user, name._1, name._2, s"${session.user}@gm.fh-koeln.de", ""))
        Ok(views.html.firstTimeInputUser(filledForm))
      }).recoverWith {
        case NonFatal(t) ⇒
          for {
            degrees ← Degrees.all()
          } yield {
            val filledForm = UserForms.userForm.fill(User(session.user, "", "", s"${session.user}@gm.fh-koeln.de", ""))
            Ok(views.html.firstTimeInputUser(filledForm))
          }
      }
    }
  }
}
