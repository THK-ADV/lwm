package models

import java.util.{ Date, UUID }

import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms._
import utils.semantic.Vocabulary.{ RDFS, OWL, LWM, RDF }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class Blacklist(semester: Resource, id: UUID = UUID.randomUUID())
case class BlacklistFormModel(semesterResource: String)
case class BlacklistDate(blacklist: Resource, date: LocalDate, id: UUID = UUID.randomUUID())
case class BlacklistDateFormModel(blacklistResource: String, semesterResource: String, date: Date)

object Blacklists {
  object Forms {
    val blacklistForm = Form(
      mapping(
        "semesterResource" -> nonEmptyText
      )(BlacklistFormModel.apply)(BlacklistFormModel.unapply)
    )

    val blacklistDateForm = Form(
      mapping(
        "blacklistResource" -> nonEmptyText,
        "semesterResource" -> nonEmptyText,
        "date" -> date
      )(BlacklistDateFormModel.apply)(BlacklistDateFormModel.unapply)
    )
  }

  import utils.Global._
  import utils.semantic.Vocabulary._
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(blacklist: Blacklist): Future[Individual] = {
    val blacklistResource = ResourceUtils.createResource(lwmNamespace, blacklist.id)
    val statements = List(
      Statement(blacklistResource, RDF.typ, LWM.Blacklist),
      Statement(blacklistResource, RDF.typ, OWL.NamedIndividual),
      Statement(blacklistResource, RDFS.label, StringLiteral(Individual(blacklist.semester).props.get(RDFS.label).get.head.asLiteral().get.decodedString)),
      Statement(blacklistResource, LWM.hasSemester, blacklist.semester),
      Statement(blacklistResource, LWM.hasId, StringLiteral(blacklist.id.toString))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(_ ⇒ Individual(blacklistResource))
  }

  def delete(blacklist: Blacklist): Future[Blacklist] = {
    val maybeBlacklist = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.Blacklist, Vocabulary.LWM.hasId, StringLiteral(blacklist.id.toString))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeBlacklist)
    val p = Promise[Blacklist]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(blacklist) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.Blacklist)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Blacklist"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.Blacklist)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ Individual(blacklist.s)).toList
    }
  }
}

object BlacklistDates {
  import utils.Global._
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(blacklist: BlacklistDate): Future[Individual] = {
    val blacklistResource = ResourceUtils.createResource(lwmNamespace, blacklist.id)
    val statements = List(
      Statement(blacklistResource, RDF.typ, LWM.BlacklistDate),
      Statement(blacklistResource, RDF.typ, OWL.NamedIndividual),
      Statement(blacklistResource, LWM.hasBlacklist, blacklist.blacklist),
      Statement(blacklistResource, LWM.hasDate, DateLiteral(blacklist.date)),
      Statement(blacklist.blacklist, LWM.hasBlacklistDate, blacklistResource),
      Statement(blacklistResource, RDFS.label, StringLiteral(s"BlacklistDate: ${blacklist.date.toString}")),
      Statement(blacklistResource, LWM.hasId, StringLiteral(blacklist.id.toString))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(_ ⇒ Individual(blacklistResource))
  }

  def delete(blacklist: BlacklistDate): Future[BlacklistDate] = {
    val maybeBlacklist = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.BlacklistDate, Vocabulary.LWM.hasId, StringLiteral(blacklist.id.toString))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeBlacklist)
    val p = Promise[BlacklistDate]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(r ⇒ r.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(blacklist) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.BlacklistDate)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a BlacklistDate"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.BlacklistDate)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ Individual(blacklist.s)).toList
    }
  }

  def getAll(blacklist: Resource): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(
      s"""
        |select ($blacklist as ?s) (${LWM.hasBlacklistDate} as ?p) ?o where{
        |$blacklist ${LWM.hasBlacklistDate} ?o
        |}
      """.stripMargin).map { stringResult ⇒
        SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ Individual(blacklist.o.asResource().get)).toList
      }
  }

  def getAllForSemester(semester: Resource): Future[List[LocalDate]] = {
    val query = s"""
        |select ?s (${LWM.hasBlacklistDate} as ?p) ?o where{
        |?s ${LWM.hasSemester} $semester .
        |?s ${LWM.hasBlacklistDate} ?d .
        |?d ${LWM.hasDate} ?o .
        |}
      """.stripMargin
    println(query)

    val list = sparqlExecutionContext.executeQuery(
      query).map { stringResult ⇒
        SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ LocalDate.parse(blacklist.o.asLiteral().get.decodedString)).toList
      }

    list.map { l ⇒ println(l) }
    list
  }
}

