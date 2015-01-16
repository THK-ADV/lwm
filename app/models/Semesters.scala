package models

import org.joda.time.LocalDate
import utils.Global.lwmNamespace
import utils.{ QueryHost, UpdateHost }
import utils.semantic.Vocabulary
import utils.semantic._
import play.api.data.Form
import play.api.data.Forms._
import utils.Implicits._
import scala.concurrent.{ Promise, Future, blocking }

sealed trait Semester {
  val year: Int
  val startDate: LocalDate
  val endDate: LocalDate
}

case class SummerSemester(year: Int) extends Semester {
  override val startDate = new LocalDate(year, 3, 1)
  override val endDate = new LocalDate(year, 8, 31)
}

case class WinterSemester(year: Int) extends Semester {
  override val startDate = new LocalDate(year, 9, 1)
  override val endDate = new LocalDate(year + 1, 2, 28)
}

case class SemesterModelForm(semester: String, year: Int)

object Semesters extends CheckedDelete {

  import scala.concurrent.ExecutionContext.Implicits.global

  val options = List("Sommersemester", "Wintersemester")

  def create(semester: Semester)(implicit updateHost: UpdateHost): Future[Resource] = {

    val p = Promise[Resource]()

    semester match {
      case ss: SummerSemester ⇒
        val id = s"Sommersemester_${semester.year}"
        val resource = Resource(s"${lwmNamespace}semesters/$id")

        blocking {
          s"""
             |${Vocabulary.defaultPrefixes}
             |
             | Insert data {
             |
             |      $resource rdf:type lwm:Semester .
             |      $resource rdf:type lwm:SummerSemester .
             |      $resource lwm:hasId "$id" .
             |      $resource lwm:hasYear "${semester.year}" .
             |      $resource lwm:hasStartDate "${semester.startDate.toString("yyyy-MM-dd")}" .
             |      $resource lwm:hasEndDate "${semester.endDate.toString("yyyy-MM-dd")}" .
             |      $resource rdfs:label "$id" .
             |
             | }
           """.stripMargin.execUpdate()
          p.success(resource)
        }

        p.future

      case ws: WinterSemester ⇒
        val id = s"Wintersemester_${semester.year}"
        val resource = Resource(s"${lwmNamespace}semesters/$id")

        blocking {
          s"""
             |${Vocabulary.defaultPrefixes}
             |
             | Insert data {
             |
             |      $resource rdf:type lwm:Semester .
             |      $resource rdf:type lwm:WinterSemester .
             |      $resource lwm:hasId "$id" .
             |      $resource lwm:hasYear "${semester.year}" .
             |      $resource lwm:hasStartDate "${semester.startDate.toString("yyyy-MM-dd")}" .
             |      $resource lwm:hasEndDate "${semester.endDate.toString("yyyy-MM-dd")}" .
             |      $resource rdfs:label "$id" .
             | }
           """.stripMargin.execUpdate()
          p.success(resource)
        }

        p.future
    }
  }

  def delete(semester: Semester)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    semester match {
      case ss: SummerSemester ⇒
        val resource = Resource(s"${lwmNamespace}semesters/Sommersemester_${semester.year}")
        delete(resource)

      case ws: WinterSemester ⇒
        val resource = Resource(s"${lwmNamespace}semesters/Wintersemester_${semester.year}")
        delete(resource)
    }
  }

  def delete(semesterId: String)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}semesters/$semesterId")
    delete(resource)
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] = Future {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | Select ?s (rdf:type as ?p) (lwm:Semester as ?o) where {
       |     ?s rdf:type lwm:Semester .
       |     optional { ?s lwm:hasYear ?year }
       |
       | } order by desc(?year)
     """.stripMargin.execSelect().map(qs ⇒ Resource(qs.data("s").toString))
  }

  override def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | ASK {
       |  $resource rdf:type lwm:Semester
       | }
     """.stripMargin.executeAsk()
  }

  def size()(implicit queryHost: QueryHost): Int = {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | Select (count(distinct ?s) as ?count) where {
       |
       |     ?s rdf:type lwm:Semester
       | }
     """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }

  def exists(semester: Semester)(implicit queryHost: QueryHost): Boolean = {

    def semesterQuery(semesterSeason: String) = {
      s"""
           |${Vocabulary.defaultPrefixes}
           |
           | ASK {
           |  ?s rdf:type lwm:Semester .
           |  ?s lwm:hasId "${semesterSeason}_${semester.year}" .
           |  ?s lwm:hasYear "${semester.year}" .
           |  ?s lwm:hasStartDate "${semester.startDate.toString("yyyy-MM-dd")}" .
           |  ?s lwm:hasEndDate "${semester.endDate.toString("yyyy-MM-dd")}" .
           | }
         """.stripMargin.executeAsk()
    }

    semester match {
      case ss: SummerSemester ⇒ semesterQuery("Sommersemester")
      case ws: WinterSemester ⇒ semesterQuery("Wintersemester")
    }
  }
}

object SemesterForm {
  val semesterForm = Form(
    mapping(
      "id" -> nonEmptyText,
      "year" -> number(min = 2012)
    )(SemesterModelForm.apply)(SemesterModelForm.unapply)
  )
}
