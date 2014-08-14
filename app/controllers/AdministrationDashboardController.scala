package controllers

import javax.mail.Message

import actors.EmailHandler
import actors.EmailHandler.EmailResponse
import org.joda.time.DateTime
import play.api.libs.concurrent.Promise
import play.api.mvc.{Action, Controller}
import play.libs.Akka
import utils.Security.Authentication

import scala.concurrent.Future


object AdministrationDashboardController extends Controller with Authentication{
  import akka.pattern.ask
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.Global._

  private val emailHandler = Akka.system.actorSelection("user/emails")

  def dashboard = hasPermissions(Permissions.AdminRole.permissions.toList : _*){session =>
    Action.async { implicit request =>
      val timeoutFuture = Promise.timeout("Oops", 15.second)
      val messageFuture = (emailHandler ? EmailHandler.MessageRequest(10)).mapTo[EmailResponse]

      Future.firstCompletedOf(Seq(messageFuture, timeoutFuture)).map {
        case EmailResponse(messages) =>
          Ok(views.html.dashboard_admin(Nil, Nil, DateTime.now(), messages))
        case s: String =>
          Ok(views.html.dashboard_admin(Nil, Nil, DateTime.now(), Nil))
      }
    }
  }
}
