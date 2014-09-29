package controllers

import models.{AssignmentAssociations, AssignmentAssociation, LabWorkGroup, LabworkGroups}
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.{StringLiteral, Resource, Individual, Vocabulary}
import utils.Global._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object EditsManagementController extends Controller with Authentication {

  def edit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val predicate = (request.body \ "predicate").as[String]
        val obj = (request.body \ "object").as[String]
        val values = (request.body \ "values").as[String].split(";")
        val statements = (request.body \ "statements").as[String].split(";")
        val resource = (request.body \ "resource").as[String]
        val old = (request.body \ "old").as[String]
        val uri = (request.body \ "redirect").as[String]
        predicate match {
          case "add" ⇒

          case "add new" ⇒
            obj match {
              case "group" ⇒
                val i = Individual(Resource(resource)).props.getOrElse(Vocabulary.LWM.hasGroup, List(Resource("")))
                val last = Individual(Resource(i(i.size - 1).value)).props.getOrElse(Vocabulary.LWM.hasGroupId, List(StringLiteral(""))).head.value
                LabworkGroups.create(LabWorkGroup(((last.charAt(0) + 1).asInstanceOf[Char]).toString, Resource(resource), Nil)).map(_ ⇒ Ok(uri))
              case "assignmentAssociation" ⇒
                val m = statements.zipWithIndex.map(p ⇒ (p._1.toLowerCase, values(p._2))).toMap
                AssignmentAssociations.create(AssignmentAssociation(Resource(m.getOrElse("hasassignment", "")), Resource(resource), m.getOrElse("haspreparationtime", "0").toInt))

            }
          case "remove" ⇒
            obj match {
              case "group" ⇒ LabworkGroups.delete(Resource(resource)).map(_ ⇒ Ok(uri))
            }
          case "update" ⇒

        }
        Future.successful(Ok(uri))
    }

  }

}
