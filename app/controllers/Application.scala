
package controllers

import models.LoginForms
import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.main(LoginForms.studentForm))
  }

}
