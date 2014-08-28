package controllers


import models.{LabWork, TimetableEntry}
import play.api.mvc.{Action, Controller}
import utils.Security.Authentication

object TimeTableController extends Controller with Authentication {

  def index() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) { session =>
    Action {
      Ok(views.html.timeTableManagement(
        List("7:30", "8:00", "8:30", "9:00", "9:30"),
        LabWork("AP1", "Algorithmen und Programmierung", 5, 8, "AP1", "Medieninformatik", "WS14/15"),
        List(TimetableEntry("AP1", List("Robert Giacinto"), "2106", "monday", "7:30"),
          TimetableEntry("AP2", List("Manuel Kirscher"), "2103", "tuesday", "9:00"),
          TimetableEntry("BS", List("Muenster Guido"), "2112", "friday", "8:00"),
          TimetableEntry("AP1", List("Manuel Krischer"), "2103", "monday", "7:30"))))
    }
  }
}
