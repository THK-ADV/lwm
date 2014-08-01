
package controllers

import models.UserForms
import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.main(UserForms.loginForm))
  }

}
