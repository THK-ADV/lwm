package models

import play.api.data.Forms._
import play.api.data._
import utils.semantic._

import scala.concurrent.Future

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
      "degree" -> text
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

case class User(  id: String,
                  firstname: String, lastname: String,
                  email: String,
                  phone: String)

object Users{
  import utils.Global._
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global


  def create(user: User): Future[Individual] = Future {
    val resource = ResourceUtils.createResource(lwmNamespace)
    val statements = List(
      Statement(resource, RDF.typ, LWM.User),
      Statement(resource, RDF.typ, OWL.NamedIndividual),
      Statement(resource, LWM.systemId, Literal(user.id)),
      Statement(resource, FOAF.firstName, Literal(user.firstname)),
      Statement(resource, FOAF.lastName, Literal(user.lastname)),
      Statement(resource, NCO.phoneNumber, Literal(user.phone)),
      Statement(resource, FOAF.mbox, Literal(user.email))
    )
    sparqlExecutionContext.executeUpdate(SPARQLBuilder.insertStatements(lwmGraph, statements: _*))
    Individual(resource)
  }

  def all(): Future[Seq[Individual]] = Future {
    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQuery(SPARQLBuilder.listIndividualsWithClass(LWM.User))).map(user => Individual(user.s))
  }

  def exists(uid: String): Boolean = sparqlExecutionContext.executeBooleanQuery(s"ASK {?s ${Vocabulary.LWM.systemId} ${Literal(uid).toQueryString}}")
}