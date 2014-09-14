import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatestplus.play._
import play.api.mvc.Results

trait LWMBaseSpec extends PlaySpec with Results with OneAppPerSuite with LWMFakeApplication with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))
}
