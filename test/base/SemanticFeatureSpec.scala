package base

import common.utils.FusekiRunner
import org.scalatest._
import org.scalatest.concurrent.{ ScalaFutures, IntegrationPatience, Eventually }
import utils.{ UpdateHost, QueryHost }

trait SemanticFeatureSpec extends WordSpecLike with Matchers with BeforeAndAfterAll with Eventually with IntegrationPatience with ScalaFutures {
  lazy val port = 3333
  lazy val serviceName = "lwm_test"
  lazy val updateHost = s"http://localhost:$port/$serviceName/update"
  lazy val queryHost = s"http://localhost:$port/$serviceName/query"
  lazy val dataInf = s"http://localhost:$port/$serviceName/data"
  implicit lazy val query = QueryHost(queryHost)
  implicit lazy val update = UpdateHost(updateHost)

  override protected def beforeAll(): Unit = {
    FusekiRunner.runOnce(serviceName, port, "test.ttl")
  }
}