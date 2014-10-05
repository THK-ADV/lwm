package controllers

import java.util.NoSuchElementException

import akka.util.Helpers.Requiring
import models.{ AssignmentAssociations, AssignmentAssociation, LabWorkGroup, LabworkGroups }
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.{ RDF, LWM }
import utils.semantic._
import utils.Global._
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object EditsManagementController extends Controller with Authentication {

  case class Component(operation: String, mappedValues: mutable.Map[Property, Any], identifier: String)

  private val deletion = LWM.property("removeEntry")

  def applyEdit = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) { implicit request ⇒
        val maybeQuery = (request.body \ "query").asOpt[String]
        val maybeUri = (request.body \ "redirect").asOpt[String]

        if (maybeQuery.isDefined && maybeUri.isDefined) {
          val component = parseQuery(maybeQuery.get)

          component.operation match {
            case "INSERT" ⇒ println("Not yet implemented")
            case "REMOVE" ⇒ println("Not yet implemented")
            case "UPDATE" ⇒ println("Not yet implemented")
            case "CREATE" ⇒ for (created ← create(component)) yield if (created) Ok(maybeUri.get)
            case "DELETE" ⇒ for (deleted ← delete(component)) yield if (deleted) Ok(maybeQuery.get)
          }
        }

        Future.successful(Redirect(routes.LabworkManagementController.index()))
      }
  }

  private def parseQuery(q: String): Component = {
    val operation = Map("INSERT" -> 1, "REMOVE" -> 1, "UPDATE" -> 1, "CREATE" -> 2, "DELETE" -> 3).filter(e ⇒ q.contains(e._1))

    if (operation.nonEmpty) {
      val o = operation.head
      o._2 match {
        case 1 ⇒
          val statements = q.split(s"${o._1}")(1).split("WITH")(0).trim.split(",")
          val values = q.split(s"${o._1}")(1).split("WITH")(1).split("IN")(0).trim.split(",")

          val mappedValues = sync(statements.zipWithIndex.map(p ⇒ (p._1.trim, values(p._2).trim)).toMap)
          val resource = q.split(s"${o._1}")(1).split("WITH")(1).split("IN")(1).trim
          Component(o._1, mappedValues, resource)

        case 2 ⇒
          val t = q.split(s"${o._1}")(1).split("HAVING")(0).toLowerCase.trim.toLowerCase
          val s = q.split(s"${o._1}")(1).split("HAVING")(1).split("WITH")(0).trim.split(",")
          val v = q.split(s"${o._1}")(1).split("HAVING")(1).split("WITH")(1).trim.split(",")
          val mappedValues = sync(s.zipWithIndex.map(p ⇒ (p._1.trim, v(p._2).trim)).toMap)
          Component(o._1, mappedValues, t)

        case 3 ⇒
          val t = q.split(s"${o._1}")(1).split("HAVING")(0).trim.toLowerCase
          val r = q.split(s"${o._1}")(1).split("HAVING")(1).trim
          val mappedValues = sync(Map(s"$deletion" -> r))
          Component(o._1, mappedValues, t)
      }
    } else {
      Component("none", mutable.Map(), "")
    }

  }

  private def sync(m: Map[String, Any]): mutable.Map[Property, Any] = {
    val newMap = collection.mutable.Map[Property, Any]()
    m.map { e ⇒
      e._1.substring(1, e._1.size - 1) match {
        case LWM.hasCourse.value     ⇒ newMap += LWM.hasCourse -> Resource(e._2.toString)
        case LWM.hasAssignment.value ⇒ newMap += LWM.hasAssignment -> StringLiteral(e._2.toString)
        case LWM.hasLabWork.value    ⇒ newMap += LWM.hasLabWork -> Resource(e._2.toString)
        case LWM.hasGroup.value      ⇒ newMap += LWM.hasGroup -> Resource(e._2.toString)
        case LWM.hasOrderId.value    ⇒ newMap += LWM.hasOrderId -> StringLiteral(e._2.toString)
        case deletion.value          ⇒ newMap += deletion -> Resource(e._2.toString)
        case _                       ⇒ println("False match")
      }
    }
    newMap
  }

  private def create(c: Component): Future[Boolean] = {
    c.identifier match {
      case "group" ⇒
        val groupQuery =
          s"""
          |select (${c.mappedValues.get(LWM.hasLabWork).get} as ?s) (${LWM.hasGroupId} as ?p) ?o where {
          | ${c.mappedValues.get(LWM.hasLabWork).get} ${LWM.hasGroup} ?group .
          | ?group ${LWM.hasGroupId} ?o
          |}
        """.stripMargin

        val groupIdFuture = sparqlExecutionContext.executeQuery(groupQuery).map { result ⇒
          SPARQLTools.statementsFromString(result).map(_.o)
        }

        val labworkGroupFuture = for {
          groupIds ← groupIdFuture
          labwork = c.mappedValues.get(LWM.hasLabWork).get.asInstanceOf[Resource]
        } yield {
          if (groupIds.nonEmpty) LabWorkGroup((groupIds(groupIds.size - 1).value.charAt(0) + 1).asInstanceOf[Char].toString, labwork, Nil)
          else LabWorkGroup('A'.toString, labwork, Nil)
        }

        for {
          lwk ← labworkGroupFuture
          c ← LabworkGroups.create(lwk)
        } yield {
          true
        }

      case "assignmentassociation" ⇒
        val labwork = c.mappedValues.get(LWM.hasLabWork).get.asInstanceOf[Resource]
        val orderId = c.mappedValues.get(LWM.hasOrderId).get.toString.toInt
        AssignmentAssociations.create(AssignmentAssociation(labwork, orderId)).map(_ ⇒ true)
    }
  }

  private def delete(c: Component): Future[Boolean] = {
    c.identifier match {
      case "group" ⇒
        val group = c.mappedValues.get(deletion).get.asInstanceOf[Resource]
        LabworkGroups.delete(group).map(_ ⇒ true)

      case "assignmentassociation" ⇒
        val association = c.mappedValues.get(deletion).get.asInstanceOf[Resource]
        AssignmentAssociations.delete(association).map(_ ⇒ true)
    }
  }
}