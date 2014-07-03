package controllers

import play.api.mvc._


case class Session(id: String)




/**
 * Session Management.
 */
object SessionManagement extends Controller{
  import util.LDAPAuthentication._

  private var sessions: Set[Session] = Set.empty

  def signIn(user: String, password: String) = Action{
    authenticate(user, password) match{
      case Left(message) =>
        Unauthorized(message)
      case Right(b) =>
        val session = getSession(user)
        sessions += session
        Ok(session.id)
    }
  }

  def getSession(user: String): Session = {
    val sessionID = s"${user.hashCode}::${System.nanoTime()}"
    Session(sessionID)
  }


}
