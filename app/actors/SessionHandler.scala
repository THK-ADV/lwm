package actors

import actors.SessionHandler._
import akka.actor.{ Actor, Props }
import controllers.Permissions
import controllers.Permissions.Role
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.DateTime
import play.api.Configuration

import scala.concurrent.Future

object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class NameRequest(user: String)

  case class LogoutRequest(sessionID: String)

  case class SessionRequest(id: String)

  case class Session(id: String, expirationDate: DateTime, user: String, groups: List[String], role: Role)

  case class SessionValidationRequest(id: String)

  case class Valid(session: Session)

  case object Invalid

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor {

  import utils.LDAPAuthentication._

  import scala.concurrent.ExecutionContext.Implicits.global

  private var sessions: Set[Session] = Set.empty

  val DN = config.getString("lwm.bindDN").get
  val GDN = config.getString("lwm.groupDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get

  def receive: Receive = {
    case AuthenticationRequest(user, password) ⇒
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)

      val requester = sender()
      authFuture.map {
        case l @ Left(error) ⇒
          requester ! l
        case Right(success) ⇒
          val sessionFuture = createSessionID(user)
          sessionFuture map { session ⇒
            sessions += session
            requester ! Right(session)
          }
      }
    case LogoutRequest(sessionID) ⇒
      sessions.find(_.id == sessionID).map(sessions -= _)
    case SessionRequest(id) ⇒
      sessions.find(_.id == id) map { session ⇒
        sender() ! session
      }
    case SessionValidationRequest(id) ⇒
      val valid = sessions.exists(_.id == id)
      if (valid) {
        sessions.find(_.id == id) map { session ⇒
          sender() ! Valid(session)
        }
      } else {
        sender() ! Invalid
      }

    case NameRequest(user) ⇒
      val requester = sender()

      val nameFuture = getName(user, bindHost, bindPort, DN)

      nameFuture.map {
        case Left(e) ⇒ println("ERROR")
        case Right(maybeName) ⇒ maybeName map { name ⇒
          requester ! (name._1, name._2)
        }
      }
  }

  private def getRoles(user: String, group: List[String]): Role = {
    // TODO Get additional role information from user details
    if (group.contains("labor")) Permissions.AdminRole else Permissions.DefaultRole
  }

  private def createSessionID(user: String): Future[Session] = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")
    val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)
    val expirationDate = DateTime.now().plusHours(lifetime)
    val groups = groupMembership(user, bindHost, bindPort, GDN)

    groups map {
      case Left(error) ⇒
        val role = getRoles(user, Nil)
        Session(sessionID, expirationDate, user, Nil, role)
      case Right(gs) ⇒
        val role = getRoles(user, gs.toList)
        Session(sessionID, expirationDate, user, gs.toList, role)
    }
  }
}