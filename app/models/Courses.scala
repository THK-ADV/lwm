package models

import play.api.data.Form
import play.api.data.Forms._
import utils.{ QueryHost, UpdateHost }
import utils.semantic._
import utils.Implicits._
import scala.concurrent.{ Future, Promise, blocking }

// Ex: Course("Algorithmen und Programmierung I", "AP1", "http://gm.fh-koeln.de/../Medieninformatik")
case class Course(name: String, id: String, degree: Resource)
case class CourseFormModel(name: String, id: String, degree: String)
/**
  * Veranstaltungen
  */
object Courses extends CheckedDelete {

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(course: Course)(implicit updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}courses/${course.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
       |${Vocabulary.defaultPrefixes}
       |
       | Insert data {
       |
       |    $resource rdf:type lwm:Course .
       |    $resource lwm:hasId "${course.id}" .
       |    $resource lwm:hasName "${course.name}" .
       |    $resource lwm:hasDegree ${course.degree} .
       |    $resource rdfs:label "${course.name}" .
       |
       | }
     """.stripMargin.execUpdate()
      p.success(resource)
    }

    p.future
  }

  def delete(courseId: String)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}courses/$courseId")
    delete(resource)
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] = Future {
    s"""
      |${Vocabulary.defaultPrefixes}
      |
      | Select ?s (rdf:type as ?p) (lwm:Course as ?o) where {
      |     ?s rdf:type lwm:Course .
      |     optional { ?s lwm:hasName ?name }
      |
      | } order by desc(?name)
    """.stripMargin.execSelect().map(qs ⇒ Resource(qs.data("s").toString))
  }

  def exists(course: Course)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | ASK {
       |  ?s lwm:hasId "${course.id}" .
       |  ?s lwm:hasName "${course.name}" .
       |  ?s lwm:hasDegree ${course.degree} .
       |  ?s rdfs:label "${course.name}" .
       | }
     """.stripMargin.executeAsk()
  }

  override def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | ASK {
       |  $resource rdf:type lwm:Course
       | }
     """.stripMargin.executeAsk()
  }

  def size()(implicit queryHost: QueryHost): Int = {
    s"""
       |${Vocabulary.defaultPrefixes}
       |
       | Select (count(distinct ?s) as ?count) where {
       |    ?s rdf:type lwm:Course
       | }
     """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }
}

object CourseForms {
  val courseForm = Form(mapping(
    "name" -> nonEmptyText,
    "id" -> nonEmptyText,
    "degree" -> nonEmptyText
  )(CourseFormModel.apply)(CourseFormModel.unapply))
}

