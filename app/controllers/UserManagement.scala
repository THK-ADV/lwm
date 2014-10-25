package controllers

import akka.util.Timeout
import models.{ UserForms, Users }
import play.api.mvc.{ Action, Controller, Result }
import utils.Global._
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDFS, FOAF, LWM, NCO }
import utils.semantic.{ SPARQLTools, Individual, Resource, StringLiteral }

import scala.concurrent.{ Future, Promise }
import scala.util.control.NonFatal

object UserManagement extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(5.seconds)

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      for (users ← Users.all()) yield Ok(views.html.userManagement(users.toList, UserForms.userForm, session))
    }
  }

  def userFirstTimeSelf = hasSession { session ⇒
    Action.async { implicit request ⇒
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          Future.successful(BadRequest(views.html.firstTimeInputUser(formWithErrors)))
        },
        user ⇒ {
          val promise = Promise[Result]()

          Users.exists(session.user).map { exists ⇒
            if (exists) {
              promise.success(Redirect(routes.AdministrationDashboardController.dashboard()))
            } else {
              if (user.id != session.user) {
                promise.success(Redirect(routes.FirstTimeSetupController.setupUser()))
              } else {
                Users.create(user).map(_ ⇒ promise.success(Redirect(routes.AdministrationDashboardController.dashboard())))
              }
            }
          }.recover {
            case NonFatal(e) ⇒ promise.success(Redirect(routes.Application.index()).withNewSession)
          }

          promise.future
        }
      )
    }
  }

  def userPost = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for (all ← Users.all()) yield {
            BadRequest(views.html.userManagement(all.toList, formWithErrors, session))
          }
        },
        user ⇒ {
          Users.create(user).map(_ ⇒ Redirect(routes.UserManagement.index()))
        }
      )
    }
  }

  def userRemoval = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) { implicit request ⇒
      val id = (request.body \ "id").as[String]
      Users.delete(Resource(id)).flatMap { deleted ⇒
        Users.all().map { all ⇒
          Redirect(routes.UserManagement.index())
        }
      }
    }
  }

  def userEdit(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒
      UserForms.userForm.bindFromRequest.fold(
        formWithErrors ⇒ {
          for {
            all ← Users.all()
          } yield {
            BadRequest(views.html.userManagement(all, formWithErrors, session))
          }
        },
        user ⇒ {
          val u = Individual(Resource(id))
          for {
            id ← u.props.getOrElse(LWM.hasGmId, List(StringLiteral("")))
            firstName ← u.props.getOrElse(FOAF.firstName, List(StringLiteral("")))
            lastName ← u.props.getOrElse(FOAF.lastName, List(StringLiteral("")))
            email ← u.props.getOrElse(FOAF.mbox, List(StringLiteral("")))
            phone ← u.props.getOrElse(NCO.phoneNumber, List(StringLiteral("")))
            label ← u.props.getOrElse(RDFS.label, List(StringLiteral("")))
          } yield {
            u.update(LWM.hasGmId, id, StringLiteral(user.id))
            u.update(FOAF.firstName, firstName, StringLiteral(user.firstname))
            u.update(FOAF.lastName, lastName, StringLiteral(user.lastname))
            u.update(FOAF.mbox, email, StringLiteral(user.email))
            u.update(NCO.phoneNumber, phone, StringLiteral(user.phone))
            u.update(RDFS.label, label, StringLiteral(s"${user.firstname} ${user.lastname}"))
          }
          Future.successful(Redirect(routes.UserManagement.index()))
        }
      )
    }
  }
}
