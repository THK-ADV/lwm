package controllers

import models.{ LabWorkGroup, LabworkGroups }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.{ Resource, Individual, Vocabulary }
import utils.Global._
import scala.concurrent.Future

/**
  * Created by root on 9/29/14.
  */
object EditsManagementController extends Controller with Authentication {

  def edit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val predicate = (request.body \ "predicate").as[String]
        val resource = (request.body \ "resource").as[String]
        val statements = (request.body \ "statements").as[String]
        val values = (request.body \ "values").as[String]
        val uri = (request.body \ "redirect").as[String]
        predicate match {
          case "add" ⇒

          case "add new" ⇒
            values match {
              case "group" ⇒
                val i = Individual(Resource(resource)).props.getOrElse(Vocabulary.LWM.hasGroup, List(Resource("")))
              //LabworkGroups.create(LabWorkGroup(i(i.size - 1).value + 1, Resource(resource), Nil))
            }
          case "remove" ⇒

          case "update" ⇒
        }
        Future.successful(Ok(uri))
    }

  }

}
