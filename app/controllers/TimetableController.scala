package controllers


import controllers.LabworkManagementController._
import controllers.StudentsManagement._
import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.Vocabulary.{FOAF, LWM}
import utils.semantic.{Vocabulary, RDFNode, Individual, Resource}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TimetableController extends Controller with Authentication {

  import utils.Global._

  def index(id: String) = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action.async { request =>
     for(supervisors <- Users.all())
     yield {
        val labWorkI = Individual(Resource(id))
        val timetable = Individual((for (i <- labWorkI.props(LWM.hasTimetable)) yield i.asResource().get).head)
        val entries = timetable.props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get)

       Ok(views.html.timeTableManagement(
          TimeSlots.slotTimeMap.values.toList.sorted,
          labWorkI, entries, supervisors.toList, TimeTableForm.timetableForm))
      }
    }
  }

  //REDIRECTS TO LABWORK PAGE. => CANNOT REFLECT LABWORK THROUGH TIMETABLE
  def timeTableEntryPost() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session =>
      Action.async {
        implicit request =>
          TimeTableForm.timetableForm.bindFromRequest.fold(
            formWithErrors => {
              for{e <- convert(formWithErrors.get)
                  supervisors <- Users.all()
              } yield {
                BadRequest(views.html.timeTableManagement(
                TimeSlots.slotTimeMap.values.toList,
                Individual(Individual(e.timetable).props(LWM.hasLabWork).head.asResource().get),
                Individual(e.timetable).props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get),
                supervisors.toList,formWithErrors))
              }
            },
            entry => {
              for{e <- convert(entry)
                  supervisors <- Users.all()
              }  yield {
                TimetableEntries.create(e)
                /*Ok(views.html.timeTableManagement(
                  TimeSlots.slotTimeMap.values.toList,
                  Individual(Individual(e.timetable).props(LWM.hasLabWork).head.asResource().get),
                  Individual(e.timetable).props.getOrElse(LWM.hasEntry, List.empty[Resource]).map(_.asResource().get),
                  supervisors.toList,TimeTableForm.timetableForm))*/
              }
              for {
                labworks <- LabWorks.all()
                courses <- Courses.all()
                degrees <- Degrees.all()
              } yield {
                Ok(views.html.labwork_management(labworks.toList, degrees.toList, courses.toList, LabWorkForms.labworkForm))
              }
            }
          )
      }
  }

      def convert(entry: TimetableEntryFormEntry): Future[TimetableEntry] = {
        for (s <- Users.all())
        yield {
          val supervisors = s.filter(i => i.props(FOAF.firstName).head.value == entry.supervisors).map(_.uri).toList

          TimetableEntry(
            Weekdays.workWeek.filter(p => p.label == entry.day).head,
            Time(entry.startTime.split(":")(0).toInt, entry.startTime.split(":")(1).toInt),
            Time(entry.endTime.split(":")(0).toInt, entry.endTime.split(":")(1).toInt) ,
            entry.room,
            supervisors,
            Resource(entry.ttId))
        }
      }



}
