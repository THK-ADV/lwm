package models

import base.SemanticFeatureSpec
import org.scalatest.Pending

class ActionSpec extends SemanticFeatureSpec {
  "Actions" should {
    "return the number of Actions" in {
      eventually {
        Actions.size should be(0)
      }
    }
    "create a new Action" in Pending
    "return true if an arbitrary resource is really an Action" in Pending
    "delete an existing Action" in Pending
    "return a list of all Actions" in Pending
  }
}
