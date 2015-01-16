package models

import play.api.data.Forms._
import play.api.data._
import utils.Implicits._
import utils.semantic._
import utils.{QueryHost, UpdateHost}

import scala.concurrent.{Future, Promise, blocking}

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

object Users extends CheckedDelete {

  import utils.semantic.Vocabulary._
  import utils.Global.lwmNamespace
  import utils.semantic.Vocabulary._

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(user: User)(implicit updateHost: UpdateHost): Future[Resource] = {

    val resource = Resource(s"${lwmNamespace}users/${user.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
      |${Vocabulary.defaulPrefixes}
      |
      |
      |insert data {
      |    $resource rdf:type lwm:User .
      |    $resource lwm:hasGmId "${user.id}" .
      |    $resource foaf:lastName "${user.lastname}" .
      |    $resource foaf:firstName "${user.firstname}" .
      |    $resource rdfs:label "${user.firstname} ${user.lastname}" .
      |    $resource nco:phoneNumber "${user.phone}" .
      |    $resource foaf:mbox "${user.email}" .
      |}
    """.stripMargin.execUpdate()
      p.success(resource)
    }

    p.future
  }

  def delete(userId: String)(implicit queryHost: QueryHost, updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${lwmNamespace}users/$userId")
    delete(resource)
  }

  def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |
      |ask {
      |   $resource rdf:type lwm:User
      |}
    """.stripMargin.executeAsk()
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] = Future {
    s"""
         |select ?s (${RDF.typ} as ?p) (${LWM.User} as ?o) where {
         | ?s ${RDF.typ} ${LWM.User} .
         | optional {?s ${FOAF.lastName} ?lastname}
         |}order by desc(?lastname)
       """.stripMargin.execSelect().map { solution ⇒
      Resource(solution.data("s").toString)
    }
  }

  def exists(uid: String)(implicit queryHost: QueryHost): Boolean = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |
      |ask {
      |   ?s lwm:hasGmId "$uid"
      |}
    """.stripMargin.executeAsk()
  }

  def possibleSubstitutes(userId: String)(implicit queryHost: QueryHost) = {
    s"""
          |${Vocabulary.defaulPrefixes}
          |
          | Select ?user ?name where {
          |    ?user rdf:type lwm:User .
          |    ?user rdfs:label ?name .
          |  filter not exists {?user lwm:hasGmId "$userId"}
          | }
        """.stripMargin.execSelect().map { solution ⇒
      val resource = solution.data("user").toString
      val name = solution.data("name").toString
      resource -> name
    }
  }

  def userMapping()(implicit queryHost: QueryHost) = {
    s"""
          |${Vocabulary.defaulPrefixes}
          |
          | Select ?user ?name where {
          |    ?user rdf:type lwm:User .
          |    ?user rdfs:label ?name
          | }
        """.stripMargin.execSelect().map { solution ⇒
      val resource = solution.data("user").toString
      val name = solution.data("name").toString
      resource -> name
    }
  }

  def size()(implicit queryHost: QueryHost): Int = {
    s"""
      |${Vocabulary.defaulPrefixes}
      |
      |select (count(distinct ?user) as ?count) {
      |   ?user rdf:type lwm:User
      |}
    """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }
}