package controllers


import models._
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication
import utils.semantic.{Resource, Individual}

object TimeTableController extends Controller with Authentication {

  import utils.Global._

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action {
      Ok(views.html.timeTableManagement(
        List(Time(7, 30), Time(8, 0), Time(8, 30), Time(9, 0), Time(9, 30)),
        LabWork("AP1", "Algorithmen und Programmierung", 5, 8, "AP1", "Medieninformatik", "WS14/15"),
        List(TimetableSlot(Weekdays.Monday, Time(7, 30), Time(9, 0), List("Robert Giacinto"), Room(Resource("2103")),Individual(Resource("AP1"))),
          TimetableSlot(Weekdays.Tuesday, Time(8, 0), Time(9, 0), List("Robert Giacinto"),Room(Resource("2105")),Individual(Resource("AP1"))),
        TimetableSlot(Weekdays.Monday, Time(7, 30), Time(9, 0), List("Manuel Krischer"), Room(Resource("2104")),Individual(Resource("BS"))),
        TimetableSlot(Weekdays.Friday, Time(7, 30), Time(9, 0), List("Guido Muenster"), Room(Resource("2103")),Individual(Resource("AP1"))))))
    }
  }
}
