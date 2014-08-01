package models

import play.api.data.Forms._
import play.api.data._

object LoginForms {
  val studentForm = Form(
    mapping(
      "user" -> text,
      "password" -> text
    )(LoginData.apply)(LoginData.unapply)
  )
}

case class LoginData(user: String, password: String)