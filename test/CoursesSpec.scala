import models.{ Courses, Course }
import org.scalatest.concurrent.{ Futures, ScalaFutures }
import org.scalatest.time.{ Millis, Seconds, Span }
import utils.semantic.Vocabulary.{ RDFS, LWM }
import utils.semantic.{Resource, StringLiteral, Statement, Individual}

class CoursesSpec extends LWMBaseSpec {
  import utils.Global._

  "Courses" should {
    "add a new Course to the ontology" in {
      val course = Course("Test Course", "tc", Resource("TestResource"))
      val returnValue = Courses.create(course)

      whenReady(returnValue) { result ⇒
        result.getClass mustEqual classOf[Individual]
        result.properties must contain(Statement(result.uri, LWM.hasId, StringLiteral(course.id)))
        result.properties must contain(Statement(result.uri, LWM.hasName, StringLiteral(course.name)))
        result.properties must contain(Statement(result.uri, RDFS.label, StringLiteral(course.name)))
      }

    }

    "remove an existing Course" in {
      val course = Course("Test Course", "tc", Resource("TestResource"))
      Courses.delete(course)
      whenReady(Courses.exists(course)){b => assert(!b)}
    }

  }
}
