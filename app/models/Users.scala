package models

import play.api.data.Forms._
import play.api.data._

object UserForms {
  val loginForm = Form(
    mapping(
      "user" -> text,
      "password" -> text
    )(LoginData.apply)(LoginData.unapply)
  )

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
}

case class LoginData(user: String, password: String)

