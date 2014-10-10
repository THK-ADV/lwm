package controllers

import models.Substitutions
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import scala.concurrent.ExecutionContext.Implicits.global

object SubstitutionsManagementController extends Controller with Authentication {
  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          for {
            substitutions ← Substitutions.all()
          } yield {
            Ok(views.html.substitution_management(substitutions, Substitutions.Forms.subsitutionForm))
          }
      }
  }
}
