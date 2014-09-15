package controllers

import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ Individual, Resource }
import utils.semantic.Vocabulary.{ LWM }
import utils.Global._
import scala.concurrent.Future

/**
  * Created by root on 9/15/14.
  */
object GroupManagementController extends Controller with Authentication {

  def index(labworkId: String, groupId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        request ⇒
          val lI = Individual(Resource(labworkId))
          val gI = Individual(Resource(groupId))
          val s = gI.props.getOrElse(LWM.hasMember, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          val a = lI.props.getOrElse(LWM.hasAssignmentAssociation, List(Resource(""))).map(r ⇒ Individual(Resource(r.value)))
          Future.successful(Ok(views.html.groups_detail_management(lI, gI, s, a)))

      }
  }
}
