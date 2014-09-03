import models.{Courses, Course}
import org.scalatest.concurrent.ScalaFutures
import utils.semantic.Vocabulary.{RDFS, LWM}
import utils.semantic.{Literal, Statement, Individual}

class CoursesSpec extends LWMBaseSpec with ScalaFutures{
  "Courses" should {
    "add a new Course to the ontology" in {
      val course = Course("Test Course 1", "tc1")
      val returnValue = Courses.create(course)

      whenReady(returnValue){result =>
        result.getClass mustEqual classOf[Individual]
        result.properties must contain (Statement(result.uri, LWM.hasId, Literal(course.id)))
        result.properties must contain (Statement(result.uri, LWM.hasName, Literal(course.name)))
        result.properties must contain (Statement(result.uri, RDFS.label, Literal(course.name)))
      }
    }
  }
}
