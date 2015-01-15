package models

import base.SemanticFeatureSpec
import utils.semantic.{ Vocabulary, Resource }
import utils.Implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class SemesterSpec extends SemanticFeatureSpec {

  val semester1 = WinterSemester(2014)
  val semester2 = SummerSemester(2014)
  val semester3 = SummerSemester(2015)
  val semester4 = WinterSemester(2015)

  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/semesters/Wintersemester_2014")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/semesters/Sommersemester_2014")
  val expectedResource3 = Resource("http://lwm.gm.fh-koeln.de/semesters/Sommersemester_2015")
  val expectedResource4 = Resource("http://lwm.gm.fh-koeln.de/semesters/Wintersemester_2015")

  "Semesters" should {
    "return the number of semesters" in {
      val expected = 0
      eventually {
        Semesters.size should be(expected)
      }
    }

    "create a new semester" in {
      val futureSemester = Semesters.create(semester1)
      whenReady(futureSemester) { semester ⇒
        semester should be(expectedResource1)
        Semesters.size should be(1)

        s"""
          |${Vocabulary.defaulPrefixes}
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:Semester .
          |    $expectedResource1 rdf:type lwm:WinterSemester .
          |    $expectedResource1 lwm:hasId ?id .
          |    $expectedResource1 lwm:hasYear ?year .
          |    $expectedResource1 lwm:hasStartDate ?startDate .
          |    $expectedResource1 lwm:hasEndDate ?endDate .
          |    $expectedResource1 rdfs:label ?label .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val year = solution.data("year").asLiteral().getInt
          val label = solution.data("label").asLiteral().getString
          val startDate = solution.data("startDate").asLiteral().getString
          val endDate = solution.data("endDate").asLiteral().getString

          (id, year, label, startDate, endDate)
        } should contain theSameElementsAs List((s"Wintersemester_${semester1.year}", semester1.year, s"Wintersemester_${semester1.year}", semester1.startDate.toString("yyyy-MM-dd"), semester1.endDate.toString("yyyy-MM-dd")))
      }
    }

    "delete an existing semester" in {
      val futureResource = Semesters.delete(semester1)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Semesters.size should be(0)
      }
    }

    "delete one particuluar Semester" in {
      "drop all".execUpdate()
      val futureSemesters = Semesters.create(semester1) :: Semesters.create(semester2) :: Semesters.create(semester3) :: Nil

      whenReady(Future.sequence(futureSemesters)) { semesters ⇒
        whenReady(Semesters.delete(semester2)) { Semester ⇒
          Semester should be(expectedResource2)
        }
      }
    }

    "check existance of a Semester" in {
      "drop all".execUpdate()
      val futureSemesters = Semesters.create(semester1) :: Semesters.create(semester4) :: Nil

      whenReady(Future.sequence(futureSemesters)) { semesters ⇒
        Semesters.exists(semester1) should be(right = true)
        Semesters.exists(semester3) should be(right = false)

      }
    }

    "return a list of all Semesters" in {
      "drop all".execUpdate()
      val futureSemesters = Semesters.create(semester1) :: Semesters.create(semester2) :: Semesters.create(semester3) :: Nil

      whenReady(Future.sequence(futureSemesters)) { semesters ⇒
        whenReady(Semesters.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "should return true if an arbitrary resource is really a semester" in {
      Semesters.check(expectedResource1) should be(right = true)
    }
  }

}
