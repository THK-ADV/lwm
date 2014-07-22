package util

import controllers.routes
import play.api.mvc._
import play.api.mvc.{Security => PlaySecurity}


object Security {

  trait Authentication {
    self: Controller =>

    import play.api.mvc.Security._

    def username(request: RequestHeader) = request.session.get(PlaySecurity.username)

    def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.index)

    def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
      Authenticated(username, onUnauthorized) { user =>
        Action(request => f(user)(request))
      }
    }
  }

}
