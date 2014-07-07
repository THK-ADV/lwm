package controllers

import play.api.mvc.{Action, Controller}


case class Session(id: String)




/**
 * Session Management.
 */
object SessionManagement extends Controller{
  import util.LDAPAuthentication._

  private var sessions: Set[Session] = Set.empty

  /*
  * {
  *   user: "bla",
  *   passwd: "hallo123"
  * }
   */
  def signIn() = Action(parse.json){ request =>
    val user = (request.body \ "user").as[String]
    val password = (request.body \ "password").as[String]

    authenticate(user, password) match{
      case Left(message) =>
        Unauthorized(message)
      case Right(b) =>
        val session = getSession(user)
        sessions += session
        Ok("").withSession(
          "connected" -> "user",
          "session" -> session.id
        )
    }
  }

  def getSession(user: String): Session = {
    val sessionID = s"${user.hashCode}::${System.nanoTime()}"
    Session(sessionID)
  }


}
