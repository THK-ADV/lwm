package controllers

import java.util.NoSuchElementException

import models.{ AssignmentAssociations, AssignmentAssociation, LabWorkGroup, LabworkGroups }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.LWM
import utils.semantic.{ StringLiteral, Resource, Individual, Vocabulary }
import utils.Global._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object EditsManagementController extends Controller with Authentication {

  def edit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val predicate = (request.body \ "predicate").as[String]
        val obj = (request.body \ "object").as[String]
        val values = (request.body \ "values").as[String].split(",")
        val statements = (request.body \ "statements").as[String].split(",")
        val resource = (request.body \ "resource").as[String]
        val old = (request.body \ "old").as[String]
        val uri = (request.body \ "redirect").as[String]

        val individual = Individual(Resource(resource))
        val mappedValues = statements.zipWithIndex.map(p ⇒ (p._1.toLowerCase.trim, values(p._2))).toMap

        predicate match {
          case "add" ⇒

          case "add new" ⇒
            obj.toLowerCase match {

              case "group" ⇒
                try {
                  val maybeGroup = individual.props(LWM.hasGroup)
                  val last = Individual(Resource(maybeGroup(maybeGroup.size - 1).value)).props.getOrElse(Vocabulary.LWM.hasGroupId, List(StringLiteral(""))).head.value
                  LabworkGroups.create(LabWorkGroup(((last.charAt(0) + 1).asInstanceOf[Char]).toString, Resource(resource), Nil)).map(_ ⇒ Ok(uri))
                } catch {
                  case e: NoSuchElementException ⇒
                    LabworkGroups.create(LabWorkGroup('A'.toString, Resource(resource), Nil)).map { p ⇒
                      individual.add(LWM.hasGroup, p.uri)
                    }.map { _ ⇒ Ok(uri) }
                }

              case "assignmentassociation" ⇒ AssignmentAssociations.create(AssignmentAssociation(Resource(resource))).map { _ ⇒ Ok(uri) }

              case "bindassociationvalues" ⇒
                individual.add(LWM.hasAssignment, Resource(mappedValues.getOrElse("hasassignment", "")))
                individual.add(LWM.hasPreparationTime, StringLiteral(mappedValues.getOrElse("haspreparationtime", "0")))
            }

          case "remove" ⇒
            obj.toLowerCase match {
              case "group" ⇒ LabworkGroups.delete(Resource(resource)).map(_ ⇒ Ok(uri))
              case "assignmentassociation" ⇒
                mappedValues.foreach { e ⇒
                  if (e._2 != "" && e._2 != " ") {
                    e._1 match {
                      case "hasassignment" ⇒
                        individual.remove(LWM.hasAssignment, Resource(e._1))
                      case "haspreparationtime" ⇒
                        individual.remove(LWM.hasPreparationTime, StringLiteral(e._1))
                    }
                  }
                }
                AssignmentAssociations.delete(Resource(resource)).map(_ ⇒ Ok(uri))
            }

          case "update" ⇒

        }
        Future.successful(Ok(uri))
    }

  }

}
