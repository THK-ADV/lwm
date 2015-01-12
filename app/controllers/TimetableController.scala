package controllers

import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.SemesterDatesGenerator
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{ Individual, Resource }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TimetableController extends Controller with Authentication {

  import utils.Global._

  def index(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session ⇒
    Action.async { implicit request ⇒

      for {
        supervisors ← Users.all()
        rooms ← Rooms.all()
      } yield {
        val labWorkI = Individual(Resource(labworkid))
        val timetable = Individual((for (i ← labWorkI.props(LWM.hasTimetable)) yield i.asResource().get).head)
        val entries = timetable.props.getOrElse(LWM.hasTimetableEntry, List.empty[Resource]).map(_.asResource().get).map(r ⇒ Individual(r))

        Ok(views.html.timetable_management(
          labWorkI,
          supervisors.toList.map(r ⇒ Individual(r)),
          TimeSlots.slotTimeMap.values.toList.sorted,
          entries,
          rooms,
          TimeTableForm.timetableForm))
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
            val maybeSemester = i.props.get(LWM.hasSemester)
            val maybeTimetable = i.props.get(LWM.hasTimetable)
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

          Future.successful(Redirect(routes.LabworkManagementController.index()))
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
                  Individual(e.timetable).props.getOrElse(LWM.hasTimetableEntry, List.empty[Resource]).map(_.asResource().get).map(r ⇒ Individual(r)),
                  rooms,
                  formWithErrors))
              }
            },
            entry ⇒ {
              convert(labworkid, entry).flatMap { e ⇒
                TimetableEntries.create(e).map(_ ⇒ Redirect(routes.TimetableController.index(labworkid)))
              }
            }
          )
      }
  }

  def convert(id: String, entry: TimetableEntryFormEntry): Future[TimetableEntry] = {
    for (s ← Users.all()) yield {
      val supervisors = s.map(r ⇒ Individual(r)).filter(i ⇒ i.uri.value == entry.supervisors).map(_.uri).toList
      val timetableId = Individual(Resource(id)).props.getOrElse(LWM.hasTimetable, List.empty[Resource]).map(_.asResource().get).head
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
                  Individual(Resource(ti.props.getOrElse(LWM.hasLabWork, List(Resource(""))).head.value)),
                  supervisors.toList.map(r ⇒ Individual(r)),
                  TimeSlots.slotTimeMap.values.toList.sorted,
                  ti.props.getOrElse(LWM.hasTimetableEntry, List(Resource(""))).map(_.asResource().get).map(r ⇒ Individual(r)),
                  rooms,
                  TimeTableForm.timetableForm))
              }
            },
            entry ⇒ {
              for {
                ss ← ei.props.get(LWM.hasSupervisor)
                rs ← ei.props.get(LWM.hasRoom)
              } yield {
                ss.map(s ⇒ ei.remove(LWM.hasSupervisor, s))
                rs.map(r ⇒ ei.remove(LWM.hasRoom, r))
              }
              ei.add(LWM.hasSupervisor, Resource(entry.supervisors))
              ei.add(LWM.hasRoom, Resource(entry.room))

              Future.successful(Redirect(routes.TimetableController.index(ti.props.getOrElse(LWM.hasLabWork, List(Resource(""))).head.value)))
            }
          )
      }
  }
}
