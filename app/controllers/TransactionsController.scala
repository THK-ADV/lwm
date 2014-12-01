package controllers

import play.api.mvc._

object TransactionsController extends Controller {
  def index() = Action { implicit request ⇒
    Ok("")
  }
}
