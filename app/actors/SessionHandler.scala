package actors

import actors.SessionHandler._
import akka.actor.{ ActorLogging, Actor, Props }
import controllers.Permissions
import controllers.Permissions.Role
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time
import org.joda.time.{ Period, DateTime }
import play.api.Configuration

import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class NameRequest(user: String)

  case class LogoutRequest(sessionID: String)

  case class SessionRequest(id: String)

  case class Session(id: String, user: String, role: Role)

  case class SessionValidationRequest(id: String)

  case class Valid(session: Session)

  case object Invalid

  private[SessionHandler] case object SessionTick

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor with ActorLogging {

  import utils.LDAPAuthentication._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private var sessions: Map[Session, DateTime] = Map.empty

  val DN = config.getString("lwm.bindDN").get
  val GDN = config.getString("lwm.groupDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get
  val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)

  context.system.scheduler.schedule(15.minutes, 15.minutes, self, SessionHandler.SessionTick)

  def receive: Receive = {
    case SessionTick ⇒
      sessions = sessions.filterNot { session ⇒
        new Period(DateTime.now(), session._2).getHours > lifetime
      }

    case AuthenticationRequest(user, password) ⇒
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)

      val requester = sender()
      authFuture.map {
        case l @ Left(error) ⇒
          requester ! l
        case Right(success) ⇒
          val sessionFuture = createSessionID(user)
          sessionFuture map { session ⇒
            sessions += (session -> DateTime.now())
            requester ! Right(session)
          }
      }
    case LogoutRequest(sessionID) ⇒
      sessions = sessions.filterNot(_._1.id == sessionID)

    case SessionRequest(id) ⇒
      sessions.find(_._1.id == id) map { session ⇒
        sessions += (session._1 -> DateTime.now())
        sender() ! session
      }

    case SessionValidationRequest(id) ⇒
      val valid = sessions.exists(_._1.id == id)
      if (valid) {
        sessions.find(_._1.id == id) map { session ⇒
          sessions += (session._1 -> DateTime.now())
          sender() ! Valid(session._1)
        }
      } else {
        sender() ! Invalid
      }

    case NameRequest(user) ⇒
      val requester = sender()

      val nameFuture = getName(user, bindHost, bindPort, DN)

      nameFuture.map {
        case Left(e) ⇒ println(e)
        case Right(maybeName) ⇒ maybeName map { name ⇒
          requester ! (name._1, name._2)
        }
      }
  }

  private def getRoles(user: String): Future[Role] = {
    val laborMemberFuture = isMemberOfGroup(user, "labor", bindHost, bindPort, GDN)

    //val hkMemberFuture = isMemberOfGroup(user, "advhk", bindHost, bindPort, GDN)

    val r = for {
      laborMember ← laborMemberFuture
      //hkMember ← hkMemberFuture
    } yield {
      var role = Permissions.DefaultRole
      laborMember match {
        case Right(member) ⇒
          if (member) role = Permissions.AdminRole
      }
      /*hkMember match {
        case Right(member) ⇒
          if (member) role = Permissions.AdminRole
      }*/
      role
    }
    r.recover {
      case NonFatal(t) ⇒ Permissions.DefaultRole
    }
  }

  private def createSessionID(user: String): Future[Session] = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")

    getRoles(user) map { role ⇒
      Session(sessionID, user, role)
    }
  }
}