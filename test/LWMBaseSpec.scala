import org.scalatestplus.play._
import play.api.mvc.Results

trait LWMBaseSpec extends PlaySpec with Results with OneAppPerSuite with LWMFakeApplication
