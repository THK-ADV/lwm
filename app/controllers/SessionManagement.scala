package controllers

import play.api.mvc.{Action, Controller}


case class Session(id: String)


/**
 * Session Management.
 */
object SessionManagement extends Controller {

  import util.LDAPAuthentication._

  private var sessions: Set[Session] = Set.empty

  /*
  * {
  *   user: "bla",
  *   password: "hallo123"
  * }
   */
  def login() = Action(parse.json) { request =>
    val user = (request.body \ "user").as[String]
    val password = (request.body \ "password").as[String]

    authenticate(user, password) match {
      case Left(message) =>
        Unauthorized(message)
      case Right(b) =>
        val session = createSessionID(user)
        sessions += session
        Ok("").withSession(
          "connected" -> "user",
          "session" -> session.id
        )
    }
  }

  def logout() = Action { request =>
    val id = request.session.get("session")

    id match {
      case Some(sid) =>
        if (sessions.exists(_.id == sid)) {
          sessions -= sessions.find(_.id == sid).get
          Ok(views.html.index("Index")).withNewSession
        } else {
          Ok(views.html.index("Index")).withNewSession
        }
      case None =>
        Ok(views.html.index("Index")).withNewSession
    }
  }

  private def createSessionID(user: String): Session = {
    val sessionID = s"${user.hashCode}::${System.nanoTime()}"
    Session(sessionID)
  }


}
