package models

import base.SemanticFeatureSpec
import utils.semantic.{ Vocabulary, Resource }
import utils.Implicits._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by root on 1/14/15.
  */
class DegreeSpec extends SemanticFeatureSpec {

  val degree1 = Degree("test_name1", "test_id_1")
  val degree2 = Degree("test_name2", "test_id_2")
  val degree3 = Degree("test_name3", "test_id_3")

  val expectedResource1 = Resource("http://lwm.gm.fh-koeln.de/degrees/test_id_1")
  val expectedResource2 = Resource("http://lwm.gm.fh-koeln.de/degrees/test_id_2")
  val expectedResource3 = Resource("http://lwm.gm.fh-koeln.de/degrees/test_id_3")

  "Degrees" should {
    "return the number of degrees" in {
      val expected = 0
      eventually {
        Degrees.size should be(expected)
      }
    }

    "create a new degree" in {
      val futureDegree = Degrees.create(degree1)
      whenReady(futureDegree) { degree ⇒
        degree should be(expectedResource1)
        Degrees.size should be(1)

        s"""
          |${Vocabulary.defaulPrefixes}
          |
          |select * where {
          |    $expectedResource1 rdf:type lwm:Degree .
          |    $expectedResource1 lwm:hasId ?id .
          |    $expectedResource1 lwm:hasName ?name .
          |    $expectedResource1 rdfs:label ?label .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val name = solution.data("name").asLiteral().getString
          val label = solution.data("label").asLiteral().getString
          (id, name, label)
        } should contain theSameElementsAs List((degree1.id, degree1.name, degree1.name))
      }
    }

    "delete an existing degree" in {
      val futureResource = Degrees.delete(degree1.id)

      whenReady(futureResource) { deleted ⇒
        deleted should be(expectedResource1)
        Degrees.size should be(0)
      }
    }

    "delete one particuluar degree" in {
      "drop all".execUpdate()
      val futureDegrees = Degrees.create(degree1) :: Degrees.create(degree2) :: Nil

      whenReady(Future.sequence(futureDegrees)) { degrees ⇒
        whenReady(Degrees.delete(degree2)) { degree ⇒
          degree should be(expectedResource2)
        }
      }
    }

    "check existance of a degree" in {
      "drop all".execUpdate()
      val futureDegrees = Degrees.create(degree1) :: Degrees.create(degree2) :: Nil

      whenReady(Future.sequence(futureDegrees)) { degrees ⇒
        Degrees.exists(degree2) should be(right = true)
        Degrees.exists(degree3) should be(right = false)

      }
    }

    "return a list of all degrees" in {
      "drop all".execUpdate()
      val futureDegrees = Degrees.create(degree1) :: Degrees.create(degree2) :: Degrees.create(degree3) :: Nil

      whenReady(Future.sequence(futureDegrees)) { degrees ⇒
        whenReady(Degrees.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "should return true if an arbitrary resource is really a degree" in {
      Degrees.check(expectedResource1) should be(right = true)
    }
  }

}
