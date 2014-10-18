package models

import play.api.data.Forms._
import play.api.data._
import utils.semantic._

import scala.concurrent.{ Promise, Future }

object UserForms {
  val loginForm = Form(
    mapping(
      "user" -> nonEmptyText,
      "password" -> nonEmptyText
    )(LoginData.apply)(LoginData.unapply)
  )

  val studentForm = Form(
    mapping(
      "id" -> nonEmptyText,
      "firstname" -> text,
      "lastname" -> text,
      "registrationNumber" -> text,
      "email" -> email,
      "phone" -> text,
      "degree" -> nonEmptyText
    )(Student.apply)(Student.unapply)
  )

  val userForm = Form(
    mapping(
      "id" -> nonEmptyText,
      "firstname" -> text,
      "lastname" -> text,
      "email" -> email,
      "phone" -> text
    )(User.apply)(User.unapply)
  )
}

case class LoginData(user: String, password: String)

case class User(id: String,
                firstname: String, lastname: String,
                email: String,
                phone: String)

object Users {
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(user: User): Future[Individual] = {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.User),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.hasGmId, StringLiteral(user.id)),
      Statement(resource, FOAF.firstName, StringLiteral(user.firstname)),
      Statement(resource, FOAF.lastName, StringLiteral(user.lastname)),
      Statement(resource, RDFS.label, StringLiteral(s"${user.firstname} ${user.lastname}")),
      Statement(resource, NCO.phoneNumber, StringLiteral(user.phone)),
      Statement(resource, FOAF.mbox, StringLiteral(user.email)),
      Statement(resource, RDFS.label, StringLiteral(s"${user.firstname} ${user.lastname}"))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(statements: _*)).map { r ⇒
      Individual(resource)
    }
  }

  def delete(user: User): Future[User] = {
    val maybeUser = SPARQLBuilder.listIndividualsWithClassAndProperty(LWM.User, Vocabulary.LWM.hasGmId, StringLiteral(user.id))
    val resultFuture = sparqlExecutionContext.executeQuery(maybeUser)
    val p = Promise[User]()
    resultFuture.map { result ⇒
      val resources = SPARQLTools.statementsFromString(result).map(u ⇒ u.s)
      resources.map { resource ⇒
        sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { _ ⇒ p.success(user) }
      }
    }
    p.future
  }

  def delete(resource: Resource): Future[Resource] = {
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(RDF.typ).contains(LWM.User)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
  }

  def all(): Future[List[Individual]] = {
    val query = s"""
         |select ?s (${RDF.typ} as ?p) (${LWM.User} as ?o) where {
         | ?s ${RDF.typ} ${LWM.User} .
         | optional {?s ${FOAF.lastName} ?lastname}
         |}order by asc(?lastname)
       """.stripMargin
    sparqlExecutionContext.executeQuery(query).map { stringResult ⇒
      SPARQLTools.statementsFromString(stringResult).map(user ⇒ Individual(user.s)).toList
    }
  }

  def exists(uid: String): Future[Boolean] = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.hasGmId} ${StringLiteral(uid).toQueryString}}")

  def substituteUserMapping(userId: String) = {
    val query =
      s"""
          |select (?user as ?s) (${RDFS.label} as ?p) (?name as ?o) where {
          |  ?user ${RDF.typ} ${LWM.User} .
          |  ?user ${RDFS.label} ?name .
          |  filter not exists {?user ${LWM.hasGmId} "$userId"}
          |}
        """.stripMargin

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
      statement.s.toString.replaceAll("<", "").replaceAll(">", "") -> statement.o.toString
    }
  }

  def userFormMapping() = {
    val query =
      s"""
          |select (?user as ?s) (${RDFS.label} as ?p) (?name as ?o) where {
          |  ?user ${RDF.typ} ${LWM.User} .
          |  ?user ${RDFS.label} ?name
          |}
        """.stripMargin

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
      statement.s.toString.replaceAll("<", "").replaceAll(">", "") -> statement.o.toString
    }
  }
}