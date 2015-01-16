package models

import java.util.UUID.fromString

import base.SemanticFeatureSpec
import utils.semantic.{ Vocabulary, Resource }
import utils.Implicits._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TimetableSpec extends SemanticFeatureSpec {

  val timetable1 = Timetable(Resource("http://labwork_1.com"), fromString("cca9c110-9d02-11e4-bd06-0800200c9a66"))
  val timetable2 = Timetable(Resource("http://labwork_1.com"), fromString("d2781ce0-9d02-11e4-bd06-0800200c9a66"))
  val timetable3 = Timetable(Resource("http://labwork_3.com"), fromString("d998c240-9d02-11e4-bd06-0800200c9a66"))

  val expectedResource1 = Resource(s"http://lwm.gm.fh-koeln.de/timetables/${fromString("cca9c110-9d02-11e4-bd06-0800200c9a66")}")
  val expectedResource2 = Resource(s"http://lwm.gm.fh-koeln.de/timetables/${fromString("d2781ce0-9d02-11e4-bd06-0800200c9a66")}")
  val expectedResource3 = Resource(s"http://lwm.gm.fh-koeln.de/timetables/${fromString("d998c240-9d02-11e4-bd06-0800200c9a66")}")

  "Timetables" should {
    "return the number of timetables" in {
      val expected = 0
      eventually {
        Timetables.size should be(expected)
      }
    }

    "create a new timetable" in {
      val futureTimetable = Timetables.create(timetable1)
      whenReady(futureTimetable) { timetable ⇒
        timetable should be(expectedResource1)
        Timetables.size should be(1)

        s"""
          |${Vocabulary.defaulPrefixes}
          |
          |select * where {
          |
          |    $expectedResource1 rdf:type lwm:Timetable .
          |    $expectedResource1 lwm:hasId ?id .
          |    $expectedResource1 lwm:hasLabWork ?labwork .
          |}
        """.stripMargin.execSelect().map { solution ⇒
          val id = solution.data("id").asLiteral().getString
          val labwork = solution.data("labwork").asResource().getURI

          (id, labwork)
        } should contain theSameElementsAs List((timetable1.id.toString, timetable1.labwork.value))
      }
    }

    "get timetable from resource" in {
      "drop all".execUpdate()
      val futureTimetables = Timetables.create(timetable1) :: Timetables.create(timetable2) :: Nil

      whenReady(Future.sequence(futureTimetables)) { timetables ⇒
        Timetables.get(expectedResource1) should be(Option(timetable1))
        Timetables.get(expectedResource3) should be(None)
      }
    }

    "return a list of all timetables" in {
      "drop all".execUpdate()
      val futureTimetables = Timetables.create(timetable1) ::
        Timetables.create(timetable2) ::
        Timetables.create(timetable3) :: Nil

      whenReady(Future.sequence(futureTimetables)) { timetables ⇒
        whenReady(Timetables.all()) { all ⇒
          all should contain theSameElementsAs List(expectedResource1, expectedResource2, expectedResource3)
        }
      }
    }

    "should return true if an arbitrary resource is really a timetable" in {
      Timetables.check(expectedResource1) should be(right = true)
    }
  }
}
