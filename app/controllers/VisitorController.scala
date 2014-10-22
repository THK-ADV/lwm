package controllers

import play.api.mvc.{ Action, Controller }

object VisitorController extends Controller {

  def impressum() = Action { implicit request ⇒
    Ok(views.html.footerInfo_impressum())
  }

  def contact() = Action { implicit request ⇒
    Ok(views.html.footerInfo_contact())
  }

}
