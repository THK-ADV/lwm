package controllers


import models.{Weekdays, Time, LabWork, TimetableEntry}
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.{Individual, Resource}

object TimetableController extends Controller with Authentication {
  import utils.Global._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action {
      Ok(views.html.timeTableManagement(
        List(Time(7, 30), Time(8, 0), Time(8, 30), Time(9, 0), Time(9, 30)),
        LabWork("AP1", "Algorithmen und Programmierung", 5, 8, "AP1", "Medieninformatik", "WS14/15"),
        List(
          TimetableEntry(Weekdays.Monday, Time(7, 30), Time(9, 30), "2110", List(Resource("")), Resource("")),
          TimetableEntry(Weekdays.Tuesday, Time(8, 30), Time(10, 30), "2110", List(Resource("")), Resource("")),
          TimetableEntry(Weekdays.Thursday, Time(7, 30), Time(9, 30), "2110", List(Resource("")), Resource(""))
        )
      ))
    }
  }
}
