package controllers


import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.Vocabulary.{RDFS, LWM}
import utils.semantic.{Individual, Resource}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TimetableController extends Controller with Authentication {

  import utils.Global._

  def index(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { request =>
     for(supervisors <- Users.all())
     yield {
        val labWorkI = Individual(Resource(labworkid))
        val timetable = Individual((for (i <- labWorkI.props(LWM.hasTimetable)) yield i.asResource().get).head)
        val entries = timetable.props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get)
       Ok(views.html.timeTableManagement(
         labWorkI,
         supervisors.toList,
         TimeSlots.slotTimeMap.values.toList.sorted,
         entries,
         TimeTableForm.timetableForm))
      }
    }
  }

  def timeTableEntryPost(labworkid: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session =>
      Action.async {
        implicit request =>
          TimeTableForm.timetableForm.bindFromRequest.fold(
            formWithErrors => {
              for{e <- convert(labworkid, formWithErrors.get)
                  supervisors <- Users.all()
              } yield {
                BadRequest(views.html.timeTableManagement(
                  Individual(Resource(labworkid)),
                  supervisors.toList,
                  TimeSlots.slotTimeMap.values.toList.sorted,
                  Individual(e.timetable).props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get),
                  formWithErrors))
              }
            },
            entry => {
              for{e <- convert(labworkid, entry)
                  supervisors <- Users.all()
              }  yield {
                TimetableEntries.create(e)
                Ok(views.html.timeTableManagement(
                  Individual(Resource(labworkid)),
                  supervisors.toList,
                  TimeSlots.slotTimeMap.values.toList.sorted,
                  Individual(e.timetable).props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get),
                  TimeTableForm.timetableForm))
              }
            }
          )
      }
  }

      def convert(id: String, entry: TimetableEntryFormEntry): Future[TimetableEntry] = {
        for (s <- Users.all())
        yield {
          val supervisors = s.filter(i => i.props(RDFS.label).head.value == entry.supervisors).map(_.uri).toList
          val timetableId = Individual(Resource(id)).props.getOrElse(LWM.hasTimetable, List.empty[Resource]).map(_.asResource().get).head

          TimetableEntry(
            Weekdays.workWeek.filter(p => p.label == entry.day).head,
            Time(entry.startTime.split(":")(0).toInt, entry.startTime.split(":")(1).toInt),
            Time(entry.endTime.split(":")(0).toInt, entry.endTime.split(":")(1).toInt) ,
            entry.room,
            supervisors,
            timetableId)
        }
      }



}
