package utils

import actors.SessionHandler
import actors.SessionHandler.{Invalid, Valid}
import akka.util.Timeout
import controllers.Permissions.Permission
import controllers.routes
import play.api.libs.iteratee.{Done, Input, Iteratee}
import play.api.mvc._
import play.libs.Akka


object Security {

  trait Authentication {
    self: Controller =>

    import akka.pattern.ask

import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    private implicit val timeout = Timeout(5.seconds)
    private val sessionsHandler = Akka.system.actorSelection("user/sessions")


    def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.index)


    def hasSession(action: SessionHandler.Session => EssentialAction): EssentialAction = EssentialAction { requestHeader =>
      val maybeToken = requestHeader.session.get("session")
      val maybeIteratee = maybeToken map { token =>
        val responseFut = (sessionsHandler ? SessionHandler.SessionValidationRequest(token)).mapTo[SessionHandler.ValidationResponse]
        val sessionFuture = for {
          response <- responseFut
        } yield {
          response match {
            case Valid(session) => Some(session)
            case Invalid => None
          }
        }

        val it = sessionFuture map { sessionOpt =>
          sessionOpt map { session =>
            action(session)(requestHeader)
          } getOrElse Done(onUnauthorized(requestHeader), Input.Empty)
        }

        Iteratee.flatten(it)
      }

      maybeIteratee.getOrElse(Done(onUnauthorized(requestHeader), Input.Empty))
    }



    def hasPermissions(permissions: Permission*)(action: SessionHandler.Session => EssentialAction): EssentialAction = hasSession { session =>
      EssentialAction { requestHeader =>
        val userPermissions = session.role.permissions

        if (permissions.containsSlice(userPermissions.toSeq)) {
          action(session)(requestHeader)
        } else {
          Done[Array[Byte], Result](onUnauthorized(requestHeader))
        }
      }
    }

  }

}
