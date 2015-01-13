package models

import base.SemanticFeatureSpec
import utils.semantic.Resource
import utils.Implicits._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class StudentSpec extends SemanticFeatureSpec {

  val student1 = Student("test_gm_1", "test_firstname", "test_lastname", "test_regnr_1", "test_email", "test_phone", "http://test1.test/test_degree")
  val student2 = Student("test_gm_2", "test_firstname", "test_lastname", "test_regnr_2", "test_email", "test_phone", "http://test1.test/test_degree")
  val student3 = Student("test_gm_3", "test_firstname", "test_lastname", "test_regnr_3", "test_email", "test_phone", "http://test1.test/test_degree")
  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/students/test_gm_1")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/students/test_gm_2")
  val expectedResource3 = Resource("http://lwm.gm.fh-koeln.de/students/test_gm_3")

  "Students" should {
    "return the number of Students" in {
      val expected = 0
      eventually {
        Students.size should be(expected)
      }
    }

    "create a new student" in {
      val futureStudent = Students.create(student1)
      whenReady(futureStudent) { student ⇒
        student should be(expectedResource1)
        Students.size should be(1)

        s"""
          |prefix lwm: <http://lwm.gm.fh-koeln.de/>
          |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |prefix owl: <http://www.w3.org/2002/07/owl#>
          |prefix foaf: <http://xmlns.com/foaf/0.1/>
          |prefix nco: <http://www.semanticdesktop.org/ontologies/nco#>
          |
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:Student .
          |    $expectedResource1 lwm:hasGmId ?id  .
          |    $expectedResource1 foaf:lastName ?lastname .
          |    $expectedResource1 foaf:firstName ?firstname .
          |    $expectedResource1 rdfs:label ?label .
          |    $expectedResource1 nco:phoneNumber ?phone .
          |    $expectedResource1 foaf:mbox ?email .
          |    $expectedResource1 lwm:hasEnrollment ?degree .
          |    $expectedResource1 lwm:hasRegistrationId ?regId
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val lastname = solution.data("lastname").asLiteral().getString
          val firstname = solution.data("firstname").asLiteral().getString
          val label = solution.data("label").asLiteral().getString
          val phone = solution.data("phone").asLiteral().getString
          val email = solution.data("email").asLiteral().getString
          val degree = solution.data("degree").asResource().getURI
          val regId = solution.data("regId").asLiteral().getString
          (id, firstname, lastname, label, phone, email, degree, regId)
        } should contain theSameElementsAs List((student1.gmId, student1.firstname, student1.lastname, s"${student1.firstname} ${student1.lastname}", student1.phone, student1.email, student1.degree, student1.registrationNumber))
      }
    }

    "delete an existing student" in {
      val futureResource = Students.delete(student1.gmId)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Students.size should be(0)
      }
    }

    "return a list of all Students" in {
      "drop all".execUpdate()
      val futureStudents = Students.create(student1) ::
        Students.create(student2) ::
        Students.create(student3) :: Nil

      whenReady(Future.sequence(futureStudents)) { students ⇒
        whenReady(Students.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "get a particuluar student" in {
      "drop all".execUpdate()
      val futureStudents = Students.create(student1) :: Students.create(student2) :: Nil

      whenReady(Future.sequence(futureStudents)) { students ⇒
        whenReady(Students.get(student1.gmId)) { student ⇒
          student should be(expectedResource1)
        }
      }
    }

    "should return true if an arbitrary resource is really a student" in {
      Students.check(expectedResource1) should be(right = true)
    }
  }

}
