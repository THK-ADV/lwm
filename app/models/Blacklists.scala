package models

import java.util.{ Date, UUID }

import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms._
import utils.semantic.Vocabulary.{ rdfs, owl, lwm, rdf }
import utils.semantic._

import scala.concurrent.{ Promise, Future }

case class Blacklist(semester: Resource, id: UUID = UUID.randomUUID())
case class BlacklistFormModel(semesterResource: String)
case class BlacklistDate(blacklist: Resource, date: LocalDate, id: UUID = UUID.randomUUID())
case class BlacklistDateFormModel(blacklistResource: String, semesterResource: String, date: LocalDate)
case class BlacklistDateRangeFormModel(blacklistResource: String, semesterResource: String, startDate: LocalDate, endDate: LocalDate)

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
        "date" -> jodaLocalDate
      )(BlacklistDateFormModel.apply)(BlacklistDateFormModel.unapply)
    )

    val blacklistDateRangeForm = Form(
      mapping(
        "blacklistResource" -> nonEmptyText,
        "semesterResource" -> nonEmptyText,
        "startDate" -> jodaLocalDate,
        "endDate" -> jodaLocalDate
      )(BlacklistDateRangeFormModel.apply)(BlacklistDateRangeFormModel.unapply)
    )
  }

  import utils.Global._
  import utils.semantic.Vocabulary._
  import scala.concurrent.ExecutionContext.Implicits.global

  def create(blacklist: Blacklist): Future[Individual] = {
    val blacklistResource = ResourceUtils.createResource(lwmNamespace, blacklist.id)
    val statements = List(
      Statement(blacklistResource, rdf.typ, lwm.Blacklist),
      Statement(blacklistResource, rdf.typ, owl.NamedIndividual),
      Statement(blacklistResource, rdfs.label, StringLiteral(Individual(blacklist.semester).props.get(rdfs.label).get.head.asLiteral().get.decodedString)),
      Statement(blacklistResource, lwm.hasSemester, blacklist.semester),
      Statement(blacklistResource, lwm.hasId, StringLiteral(blacklist.id.toString))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(_ ⇒ Individual(blacklistResource))
  }

  def delete(blacklist: Blacklist): Future[Blacklist] = {
    val maybeBlacklist = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.Blacklist, Vocabulary.lwm.hasId, StringLiteral(blacklist.id.toString))
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
    if (individual.props(rdf.typ).contains(lwm.Blacklist)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a Blacklist"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.Blacklist)).map { stringResult ⇒
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
      Statement(blacklistResource, rdf.typ, lwm.BlacklistDate),
      Statement(blacklistResource, rdf.typ, owl.NamedIndividual),
      Statement(blacklistResource, lwm.hasBlacklist, blacklist.blacklist),
      Statement(blacklistResource, lwm.hasDate, DateLiteral(blacklist.date)),
      Statement(blacklist.blacklist, lwm.hasBlacklistDate, blacklistResource),
      Statement(blacklistResource, rdfs.label, StringLiteral(s"BlacklistDate: ${blacklist.date.toString}")),
      Statement(blacklistResource, lwm.hasId, StringLiteral(blacklist.id.toString))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map(_ ⇒ Individual(blacklistResource))
  }

  def delete(blacklist: BlacklistDate): Future[BlacklistDate] = {
    val maybeBlacklist = SPARQLBuilder.listIndividualsWithClassAndProperty(lwm.BlacklistDate, Vocabulary.lwm.hasId, StringLiteral(blacklist.id.toString))
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
    if (individual.props(rdf.typ).contains(lwm.BlacklistDate)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    } else {
      p.failure(new IllegalArgumentException("Resource is not a BlacklistDate"))
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(lwm.BlacklistDate)).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ Individual(blacklist.s)).toList
    }
  }

  def getAll(blacklist: Resource): Future[List[Individual]] = {
    sparqlExecutionContext.executeQuery(
      s"""
        |select ($blacklist as ?s) (${lwm.hasBlacklistDate} as ?p) ?o where{
        |$blacklist ${lwm.hasBlacklistDate} ?o
        |}
      """.stripMargin).map { stringResult ⇒
        SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ Individual(blacklist.o.asResource().get)).toList
      }
  }

  def getAllForSemester(semester: Resource): Future[List[LocalDate]] = {
    val query = s"""
        |select ?s (${lwm.hasBlacklistDate} as ?p) ?o where{
        |?s ${lwm.hasSemester} $semester .
        |?s ${lwm.hasBlacklistDate} ?d .
        |?d ${lwm.hasDate} ?o .
        |}
      """.stripMargin

    val list = sparqlExecutionContext.executeQuery(
      query).map { stringResult ⇒
        SPARQLTools.statementsFromString(stringResult).map(blacklist ⇒ LocalDate.parse(blacklist.o.asLiteral().get.decodedString)).toList
      }
    list
  }
}

