package controllers

import models.Transactions
import play.api.mvc._
import utils.Security.Authentication

object TransactionsController extends Controller with Authentication {
  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒

    Action { implicit request ⇒
      Ok(views.html.transactionsListing(Transactions.all()))
    }
  }
}
