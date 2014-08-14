package models

import play.api.data.Form
import play.api.data.Forms._

case class Course(courseID: String, name: String)

case class CourseApplication(courseID: String, gmID: String)


object CourseForms{
  val loginForm = Form(
    mapping(
      "courseID" -> nonEmptyText,
      "gmID" -> nonEmptyText
    )(CourseApplication.apply)(CourseApplication.unapply)
  )
}


object Courses {

}
