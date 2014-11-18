package controllers

import models._
import play.api.mvc.{ Action, Controller }
import utils.Security.Authentication
import utils.semantic.Vocabulary.LWM

import scala.concurrent.Future
import scala.util.Random

object TestDataController extends Controller with Authentication {

  import scala.concurrent.ExecutionContext.Implicits.global

  def generateTestData() = hasPermissions(Permissions.AdminRole.permissions.toList: _*) {
    session ⇒
      Action.async {
        implicit request ⇒

          val semesterFuture = Semesters.create(WinterSemester(2014))

          val courseFuture = Courses.create(Course("Algorithmen und Programmierung 1", "AP1", LWM.MediaInformaticsBachelor))

          val labworkFuture = for {
            semester ← semesterFuture
            course ← courseFuture
          } yield LabWorks.create(LabWork(course.uri, semester.uri))

          val studentsAffeFutures = for (i ← 1 to 500) yield Students.create(Student(
            s"mi_$i",
            s"Random_Bobbi_$i",
            s"Affe",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_$i@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))

          val studentsSchweinFutures = for (i ← 501 to 1000) yield Students.create(Student(
            s"mi_$i",
            s"Random_Schwein_$i",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_$i@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))

          for {
            affeFuture ← studentsAffeFutures
          } yield {
            for {
              affe ← affeFuture
              lwf ← labworkFuture
              lw ← lwf
            } yield LabworkApplications.create(LabworkApplication(affe.uri, lw.uri, Nil))
          }

          for {
            schweinFutures ← studentsSchweinFutures
          } yield {
            for {
              affe ← schweinFutures
              lwf ← labworkFuture
              lw ← lwf
            } yield LabworkApplications.create(LabworkApplication(affe.uri, lw.uri, Nil))
          }

          val student1 = Students.create(Student(
            s"mi_1111",
            s"Random_Schwein_1111",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))
          val student2 = Students.create(Student(
            s"mi_1112",
            s"Random_Schwein_1112",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))
          val student3 = Students.create(Student(
            s"mi_1113",
            s"Random_Schwein_1113",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))
          val student4 = Students.create(Student(
            s"mi_1114",
            s"Random_Schwein_1114",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))

          val student5 = Students.create(Student(
            s"mi_1115",
            s"Random_Schwein_1115",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))
          val student6 = Students.create(Student(
            s"mi_1116",
            s"Random_Schwein_1116",
            s"Schweini",
            s"${Random.nextInt(10000) + 11000000}",
            s"random_email_1111@gm.fh-koeln.de",
            s"${Random.nextInt(7726262)}",
            s"${LWM.MediaInformaticsBachelor.value}"
          ))

          for {
            s1 ← student1
            s2 ← student2
            s3 ← student3
            s4 ← student4
            s5 ← student5
            s6 ← student6
            lwf ← labworkFuture
            lw ← lwf
          } yield {
            LabworkApplications.create(LabworkApplication(s1.uri, lw.uri, List(s5.uri, s3.uri, s4.uri)))
            LabworkApplications.create(LabworkApplication(s2.uri, lw.uri, List(s1.uri, s3.uri, s4.uri)))
            LabworkApplications.create(LabworkApplication(s3.uri, lw.uri, List(s2.uri, s1.uri, s4.uri)))
            LabworkApplications.create(LabworkApplication(s4.uri, lw.uri, List(s2.uri, s3.uri, s1.uri)))
            LabworkApplications.create(LabworkApplication(s5.uri, lw.uri, List(s1.uri, s6.uri)))
            LabworkApplications.create(LabworkApplication(s6.uri, lw.uri, List(s6.uri)))
          }

          Future.successful(Redirect(routes.StudentsManagement.index("1")))
      }
  }
}
