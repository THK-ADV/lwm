package models

import base.SemanticFeatureSpec
import utils.semantic.{ Vocabulary, Resource }
import utils.Implicits._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by root on 1/15/15.
  */
class CourseSpec extends SemanticFeatureSpec {

  val course1 = Course("test_name1", "test_id_1", Resource("http://test_degree.com"))
  val course2 = Course("test_name2", "test_id_2", Resource("http://test_degree.com"))
  val course3 = Course("test_name3", "test_id_3", Resource("http://test_degree.com"))

  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/courses/test_id_1")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/courses/test_id_2")
  val expectedResource3 = Resource("http://lwm.gm.fh-koeln.de/courses/test_id_3")

  "courses" should {
    "return the number of courses" in {
      val expected = 0
      eventually {
        Courses.size should be(expected)
      }
    }

    "create a new course" in {
      val futureCourse = Courses.create(course1)
      whenReady(futureCourse) { course ⇒
        course should be(expectedResource1)
        Courses.size should be(1)

        s"""
          |${Vocabulary.defaultPrefixes}
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:Course .
          |    $expectedResource1 lwm:hasId ?id .
          |    $expectedResource1 lwm:hasName ?name .
          |    $expectedResource1 lwm:hasDegree ?degree .
          |    $expectedResource1 rdfs:label ?label .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val name = solution.data("name").asLiteral().getString
          val label = solution.data("label").asLiteral().getString
          val degree = solution.data("degree").asResource().getURI
          (id, name, label, degree)
        } should contain theSameElementsAs List((course1.id, course1.name, course1.name, course1.degree.value))
      }
    }

    "delete an existing course" in {
      val futureResource = Courses.delete(course1.id)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Courses.size should be(0)
      }
    }

    "check existance of a course" in {
      "drop all".execUpdate()
      val futureCourses = Courses.create(course1) :: Courses.create(course2) :: Nil

      whenReady(Future.sequence(futureCourses)) { courses ⇒
        Courses.exists(course2) should be(right = true)
        Courses.exists(course3) should be(right = false)

      }
    }

    "return a list of all courses" in {
      "drop all".execUpdate()
      val futureCourses = Courses.create(course1) :: Courses.create(course2) :: Courses.create(course3) :: Nil

      whenReady(Future.sequence(futureCourses)) { courses ⇒
        whenReady(Courses.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "should return true if an arbitrary resource is really a course" in {
      Courses.check(expectedResource1) should be(right = true)
    }
  }

}
