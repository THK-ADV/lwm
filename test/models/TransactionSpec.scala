package models

import base.SemanticFeatureSpec
import org.scalatest.Pending

class TransactionSpec extends SemanticFeatureSpec {
  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.Implicits._

  "Transactions" should {
    "return the number of transactions" in {
      eventually {
        Transactions.size should be(0)
      }
    }

    "return true if an arbitrary resource is really a Transaction" in Pending
    "delete an existing Transaction" in Pending
    "return a list of all Transactions" in Pending
    "return a list of all Transactions for a given User" in Pending
    "return a list of all Transactions for a given Student" in Pending
    "return a list of all Transaction for a given Labwork" in Pending
  }
}
