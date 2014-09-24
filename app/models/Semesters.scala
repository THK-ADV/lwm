package models

import java.util.UUID

import com.hp.hpl.jena.rdf.model.AnonId
import org.joda.time.{ LocalDate, DateTime }
import play.api.data.Form
import utils.Global._
import utils.semantic.Vocabulary.{ RDFS, OWL, LWM, RDF }
import utils.semantic._
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.{ Promise, Future }

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

object Semesters {
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(semester: Semester) = {

    def startEndList(res: Resource) = List(
      Statement(res, LWM.hasStartDate, DateLiteral(semester.startDate)),
      Statement(res, LWM.hasEndDate, DateLiteral(semester.endDate))
    )
    val semesterStatement = semester match {
      case ss: SummerSemester ⇒
        val id = s"Sommersemester_${semester.year}"
        val semesterResource = Resource(s"$lwmNamespace$id")
        val sts = List(Statement(semesterResource, LWM.hasYear, StringLiteral(s"${semester.year}")), Statement(semesterResource, RDF.typ, OWL.NamedIndividual), Statement(semesterResource, LWM.hasId, StringLiteral(id)), Statement(semesterResource, RDF.typ, LWM.SummerSemester), Statement(semesterResource, RDF.typ, LWM.Semester), Statement(semesterResource, RDFS.label, StringLiteral(s"Sommersemester ${semester.year}")))
        (semesterResource, sts ++ startEndList(semesterResource))
      case ws: WinterSemester ⇒
        val id = s"Wintersemester${semester.year}"
        val semesterResource = Resource(s"$lwmNamespace$id")
        val sts = List(Statement(semesterResource, LWM.hasYear, StringLiteral(s"${semester.year}")), Statement(semesterResource, RDF.typ, OWL.NamedIndividual), Statement(semesterResource, LWM.hasId, StringLiteral(id)), Statement(semesterResource, RDF.typ, LWM.WinterSemester), Statement(semesterResource, RDF.typ, LWM.Semester), Statement(semesterResource, RDFS.label, StringLiteral(s"Wintersemester ${semester.year}")))
        (semesterResource, sts ++ startEndList(semesterResource))
    }

    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(semesterStatement._2: _*)).map(_ ⇒ Individual(semesterStatement._1))
  }

  def delete(semester: Semester): Future[Semester] = {
    val p = Promise[Semester]()
    semester match {
      case ss: SummerSemester ⇒
        val id = s"Sommersemester_${semester.year}"
        val maybeSemester = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.SummerSemester, Vocabulary.LWM.hasId, StringLiteral(id))
        val resultFuture = sparqlExecutionContext.executeQuery(maybeSemester)

        resultFuture.map { result ⇒
          val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
          resources.map { resource ⇒
            sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(semester) }
          }
        }
      case ws: WinterSemester ⇒
        val id = s"Wintersemester${semester.year}"
        val maybeSemester = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.SummerSemester, Vocabulary.LWM.hasId, StringLiteral(id))
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
