package models

import java.util.UUID

import com.hp.hpl.jena.rdf.model.AnonId
import play.api.data.Form
import utils.Global._
import utils.semantic.Vocabulary.{ RDFS, OWL, LWM, RDF }
import utils.semantic._
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.{ Promise, Future }

sealed trait Semester {
  val year: Int
}

case class SummerSemester(year: Int) extends Semester

case class WinterSemester(year: Int) extends Semester

case class SemesterModelForm(semester: String, year: Int)

object Semesters {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(semester: Semester) = {
    val semesterStatement = semester match {
      case ss: SummerSemester ⇒
        val id = s"Sommersemester_${semester.year}"
        val semesterResource = Resource(s"$lwmNamespace$id")
        val sts = List(Statement(semesterResource, LWM.hasYear, Literal(s"${semester.year}")), Statement(semesterResource, RDF.typ, OWL.NamedIndividual), Statement(semesterResource, LWM.hasId, Literal(id)), Statement(semesterResource, RDF.typ, LWM.SummerSemester), Statement(semesterResource, RDF.typ, LWM.Semester), Statement(semesterResource, RDFS.label, Literal(s"Sommersemester ${semester.year}")))
        (semesterResource, sts)
      case ws: WinterSemester ⇒
        val id = s"Wintersemester${semester.year}"
        val semesterResource = Resource(s"$lwmNamespace$id")
        val sts = List(Statement(semesterResource, LWM.hasYear, Literal(s"${semester.year}")), Statement(semesterResource, RDF.typ, OWL.NamedIndividual), Statement(semesterResource, LWM.hasId, Literal(id)), Statement(semesterResource, RDF.typ, LWM.WinterSemester), Statement(semesterResource, RDF.typ, LWM.Semester), Statement(semesterResource, RDFS.label, Literal(s"Wintersemester ${semester.year}")))
        (semesterResource, sts)
    }

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(semesterStatement._2: _*)).map(_ ⇒ Individual(semesterStatement._1))
  }

  def delete(semester: Semester): Future[Semester] = {
    val p = Promise[Semester]()
    semester match {
      case ss: SummerSemester ⇒
        val id = s"Sommersemester_${semester.year}"
        val maybeSemester = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.SummerSemester, Vocabulary.LWM.hasId, Literal(id))
        val resultFuture = sparqlExecutionContext.executeQuery(maybeSemester)

        resultFuture.map { result ⇒
          val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
          resources.map { resource ⇒
            sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(semester) }
          }
        }
      case ws: WinterSemester ⇒
        val id = s"Wintersemester${semester.year}"
        val maybeSemester = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.SummerSemester, Vocabulary.LWM.hasId, Literal(id))
        val resultFuture = sparqlExecutionContext.executeQuery(maybeSemester)

        resultFuture.map { result ⇒
          val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
          resources.map { resource ⇒
            sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(semester) }
          }
        }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Semester) || individual.props(RDF.typ).contains(LWM.SummerSemester) || individual.props(RDF.typ).contains(LWM.WinterSemester)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Semester"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Semester)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(semester ⇒ Individual(semester.s)).toList
    }
  }

  val options = List("Sommersemester", "Wintersemester")

}

object SemesterForm {
  val semesterForm = Form(
    mapping(
      "id" -> nonEmptyText,
      "year" -> number(min = 2012)
    )(SemesterModelForm.apply)(SemesterModelForm.unapply)
  )
}
