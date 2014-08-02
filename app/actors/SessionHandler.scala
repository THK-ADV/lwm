package actors

import actors.SessionHandler._
import akka.actor.{Actor, Props}
import controllers.Permissions
import controllers.Permissions.Role
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.DateTime
import play.api.Configuration

import scala.concurrent.Future


object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class LogoutRequest(sessionID: String)

  case class SessionRequest(id: String)

  case class Session(id: String, expirationDate: DateTime, user: String, groups: List[String], role: Role)

  case class SessionValidationRequest(id: String)

  trait ValidationResponse
  case class Valid(session: Session) extends ValidationResponse

  case object Invalid extends ValidationResponse

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
    case AuthenticationRequest(user, password) =>
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)

      val requester = sender()
      authFuture.map {
        case l@Left(error) =>
          requester ! l
        case Right(success) =>
          val sessionFuture = createSessionID(user)
          sessionFuture map { session =>
            sessions += session
            requester ! Right(session)
          }
      }
    case LogoutRequest(sessionID) =>
      sessions.find(_.id == sessionID).map(sessions -= _)
    case SessionRequest(id) =>
      sessions.find(_.id == id) map { session =>
        sender() ! session
      }
    case SessionValidationRequest(id) =>
      val valid = sessions.exists(_.id == id)
      if(valid){
        sessions.find(_.id == id) map { session =>
          sender() ! Valid(session)
        }
      }else{
        sender() ! Invalid
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
      case Left(error) =>
        val role = getRoles(user, Nil)
        Session(sessionID, expirationDate, user, Nil, role)
      case Right(gs) =>
        val role = getRoles(user, gs.toList)
        Session(sessionID, expirationDate, user, gs.toList, role)
    }
  }
}