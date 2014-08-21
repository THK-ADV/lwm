package models

import play.api.data.Form
import play.api.data.Forms._

case class Course(courseID: String, name: String)

case class CourseApplication(courseID: String, gmID: String)

/**
 * An assignment group.
 * @param groupID id of this group
 * @param courseID id of this associated course
 * @param students gmIds of the students
 */
case class CourseGroup(groupID: String, courseID: String, students: List[String])


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
