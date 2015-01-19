package actors

import actors.SessionHandler._
import akka.actor.{ ActorLogging, Actor, Props }
import controllers.Permissions
import controllers.Permissions.Role
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time
import org.joda.time.{ Period, DateTime }
import play.api.Play.current
import play.api.{ Play, Configuration }
import utils.BreadCrumbKeeper

import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class NameRequest(user: String)

  case class LogoutRequest(sessionID: String)

  case class SessionRequest(id: String)

  case class Session(id: String, user: String, role: Role, breadcrumbKeeper: BreadCrumbKeeper)

  case class SessionValidationRequest(id: String)

  case class Valid(session: Session)

  case object Invalid

  case object OnlineUserCountRequest

  case object OnlineStudentCountRequest

  case class OnlineUserCountChange(count: Int)

  case class OnlineStudentCountChange(count: Int)

  private[SessionHandler] case object SessionTick

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor with ActorLogging {

  import utils.LDAPAuthentication._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  var sessions: Map[Session, DateTime] = Map.empty

  val DN = config.getString("lwm.bindDN").get
  val GDN = config.getString("lwm.groupDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get
  val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)

  context.system.scheduler.schedule(1.minutes, 1.minutes, self, SessionHandler.SessionTick)

  def receive: Receive = {

    case OnlineUserCountRequest ⇒
      val users = sessions.keys.count(_.role.permissions.contains(Permissions.ScheduleAssociationModification))
      sender() ! OnlineUserCountChange(users)

    case OnlineStudentCountRequest ⇒
      val users = sessions.keys.count(_.role.permissions.contains(Permissions.ScheduleAssociationModification))
      val students = sessions.keys.size - users
      sender() ! OnlineStudentCountChange(students)

    case SessionTick ⇒
      sessions = sessions.filterNot { session ⇒
        new Period(DateTime.now(), session._2).getMinutes > lifetime
      }

      val users = sessions.keys.count(_.role.permissions.contains(Permissions.ScheduleAssociationModification))
      val students = sessions.keys.size - users

      context.system.eventStream.publish(OnlineStudentCountChange(students))
      context.system.eventStream.publish(OnlineUserCountChange(users))

    case AuthenticationRequest(user, password) ⇒
      val requester = sender()

      if (Play.isDev) {
        val sessionFuture = createSessionID(user)
        sessionFuture map { session ⇒
          sessions += (session -> DateTime.now())
          requester ! Right(session)
        }
      } else {
        val authFuture = authenticate(user, password, bindHost, bindPort, DN)

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
      }
      val users = sessions.keys.count(_.role.permissions.contains(Permissions.ScheduleAssociationModification))
      val students = sessions.keys.size - users

      context.system.eventStream.publish(OnlineStudentCountChange(students))
      context.system.eventStream.publish(OnlineUserCountChange(users))

    case LogoutRequest(sessionID) ⇒
      sessions = sessions.filterNot(_._1.id == sessionID)
      val users = sessions.keys.count(_.role.permissions.contains(Permissions.ScheduleAssociationModification))
      val students = sessions.keys.size - users

      context.system.eventStream.publish(OnlineStudentCountChange(students))
      context.system.eventStream.publish(OnlineUserCountChange(users))

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
        case Left(e) ⇒
        case Right(maybeName) ⇒ maybeName map { name ⇒
          requester ! (name._1, name._2)
        }
      }
  }

  private def getRoles(user: String): Future[Role] = {

    if (Play.isDev) {
      Future.successful(Permissions.DefaultRole)
    } else {
      val laborMemberFuture = isMemberOfGroup(user, "advlabor", bindHost, bindPort, GDN)
      val hkMemberFuture = isMemberOfGroup(user, "advhk", bindHost, bindPort, GDN)

      val r = for {
        laborMember ← laborMemberFuture
        hkMember ← hkMemberFuture
      } yield {
        var role = Permissions.DefaultRole
        laborMember match {
          case Right(member) ⇒
            if (member) role = Permissions.AdminRole
          case Left(error) ⇒ role = Permissions.DefaultRole
        }
        hkMember match {
          case Right(member) ⇒
            if (member) role = Permissions.AdminRole
        }
        role
      }

      r.recover {
        case NonFatal(t) ⇒ Permissions.DefaultRole
      }
    }
  }

  private def createSessionID(user: String): Future[Session] = {
    val sessionID = DigestUtils.sha1Hex(s"$user::${System.nanoTime()}")
    val breadcrumbs = new utils.BreadCrumbKeeper

    if (Play.isDev) {
      Future.successful(Session(sessionID, user, Permissions.DefaultRole, breadcrumbs))
    } else {
      getRoles(user) map { role ⇒
        Session(sessionID, user, role, breadcrumbs)
      }
    }
  }
}