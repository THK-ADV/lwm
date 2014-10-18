package controllers

import actors.SupervisionSocketActor
import org.joda.time.LocalDate
import play.api.libs.json.JsValue
import play.api.mvc._
import utils.Security.Authentication
import utils.semantic.Resource
import play.api.Play.current

import scala.concurrent.Future

object SupervisionController extends Controller with Authentication {

  def superviseSocket = WebSocket.acceptWithActor[JsValue, JsValue] { request ⇒
    out ⇒
      println("Creating actor")
      SupervisionSocketActor.props(out)
  }

  def supervise(id: String, date: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      Future.successful(Ok(views.html.supervision(Resource(id), LocalDate.parse(date))))
    }
  }
}