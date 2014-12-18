package acceptance

import base.FeatureSpecBase

class SupervisionUAT extends FeatureSpecBase {
  feature("Supervision of students") {
    scenario("Student is not attending the labwork and is not excused") {
      Given("The supervision page for this labwork assignment is open")
      When("the supervisor calls the name of a student and the student is not attending")
      Then("the student is marked as absent")
    }
  }
}
