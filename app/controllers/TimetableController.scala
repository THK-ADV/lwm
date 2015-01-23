package controllers

import controllers.SessionManagement._
import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.SemesterDatesGenerator
import utils.semantic.Vocabulary.{ rdfs, lwm }
import utils.semantic.{ Individual, Resource }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

object TimetableController extends Controller with Authentication {

  import utils.Global._

  def index(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      (for {
        supervisors ← Users.all()
        rooms ← Rooms.all()
      } yield {
        val labWorkI = Individual(Resource(labworkid))
        val timetable = Individual((for (i ← labWorkI.props(lwm.hasTimetable)) yield i.asResource().get).head)
        val entries = timetable.props.getOrElse(lwm.hasTimetableEntry, List.empty[Resource]).map(_.asResource().get).map(r ⇒ Individual(r))

        Ok(views.html.timetable_management(
          labWorkI,
          supervisors.toList.map(r ⇒ Individual(r)),
          TimeSlots.slotTimeMap.values.toList.sorted,
          entries,
          rooms.map(e ⇒ Individual(e)),
          TimeTableForm.timetableForm))
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
  }

  def generateSchedule() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async(parse.json) {
        implicit request ⇒
          val maybeLabworkid = (request.body \ "id").asOpt[String]
          val r = maybeLabworkid.map { labworkid ⇒
            val i = Individual(Resource(labworkid))
            val maybeSemester = i.props.get(lwm.hasSemester)
            val maybeTimetable = i.props.get(lwm.hasTimetable)
            val scheduleFuture = for {
              timetableList ← maybeTimetable
              semesterList ← maybeSemester
            } yield {
              val timetable = timetableList.head.asResource().get
              val semester = semesterList.head.asResource().get
              SemesterDatesGenerator.create(timetable, semester)
            }
            scheduleFuture
          }

          Future(Redirect(routes.LabworkManagementController.index())).recover {
            case NonFatal(e) ⇒
              InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
          }
      }
  }

  def entryPost(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          TimeTableForm.timetableForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                e ← convert(labworkid, formWithErrors.get)
                supervisors ← Users.all()
                rooms ← Rooms.all()
              } yield {
                BadRequest(views.html.timetable_management(
                  Individual(Resource(labworkid)),
                  supervisors.toList.map(r ⇒ Individual(r)),
                  TimeSlots.slotTimeMap.values.toList.sorted,
                  Individual(e.timetable).props.getOrElse(lwm.hasTimetableEntry, List.empty[Resource]).map(_.asResource().get).map(r ⇒ Individual(r)),
                  rooms.map(e ⇒ Individual(e)),
                  formWithErrors))
              }
            },
            entry ⇒ {
              convert(labworkid, entry).flatMap { e ⇒
                TimetableEntries.create(e).map(_ ⇒ Redirect(routes.TimetableController.index(labworkid)))
              }
            }
          ).recover {
              case NonFatal(e) ⇒
                InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
            }
      }
  }

  def convert(id: String, entry: TimetableEntryFormEntry): Future[TimetableEntry] = {

    for (s ← Users.all()) yield {
      val supervisors = s.map(r ⇒ Individual(r)).filter(i ⇒ i.uri.value == entry.supervisors).map(_.uri).toList
      val timetableId = Individual(Resource(id)).props.getOrElse(lwm.hasTimetable, List.empty[Resource]).map(_.asResource().get).head
      TimetableEntry(
        Weekdays.workWeek.values.filter(p ⇒ p.label == entry.day).head,
        Time(entry.startTime.split(":")(0).toInt, entry.startTime.split(":")(1).toInt),
        Time(entry.endTime.split(":")(0).toInt, entry.endTime.split(":")(1).toInt),
        Resource(entry.room),
        supervisors,
        timetableId)
    }
  }

  def entryRemoval(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async(parse.json) {
      implicit request ⇒
        val entryId = (request.body \ "eId").as[String]
        val labId = (request.body \ "lId").as[String]
        TimetableEntries.delete(Resource(entryId)).map { _ ⇒
          Redirect(routes.TimetableController.index(labId))
        }
    }
  }

  def entryEdit(timetableId: String, entryId: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒
          val ti = Individual(Resource(timetableId))
          val ei = Individual(Resource(entryId))
          TimeTableForm.timetableEditForm.bindFromRequest.fold(
            formWithErrors ⇒ {
              for {
                supervisors ← Users.all()
                rooms ← Rooms.all()
              } yield {
                BadRequest(views.html.timetable_management(
                  Individual(Resource(ti.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head.value)),
                  supervisors.toList.map(r ⇒ Individual(r)),
                  TimeSlots.slotTimeMap.values.toList.sorted,
                  ti.props.getOrElse(lwm.hasTimetableEntry, List(Resource(""))).map(_.asResource().get).map(r ⇒ Individual(r)),
                  rooms.map(e ⇒ Individual(e)),
                  TimeTableForm.timetableForm))
              }
            },
            entry ⇒ {
              for {
                ss ← ei.props.get(lwm.hasSupervisor)
                rs ← ei.props.get(lwm.hasRoom)
              } yield {
                ss.map(s ⇒ ei.remove(lwm.hasSupervisor, s))
                rs.map(r ⇒ ei.remove(lwm.hasRoom, r))
              }
              ei.add(lwm.hasSupervisor, Resource(entry.supervisors))
              ei.add(lwm.hasRoom, Resource(entry.room))

              Future.successful(Redirect(routes.TimetableController.index(ti.props.getOrElse(lwm.hasLabWork, List(Resource(""))).head.value)))

            }
          )
      }
  }
}
