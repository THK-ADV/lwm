package models

import play.api.data.Forms._
import play.api.data._
import utils.{ Global, QueryHost, UpdateHost }
import utils.semantic._
import utils.Implicits._

import scala.concurrent.{ Promise, Future, blocking }

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

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(user: User)(implicit updateHost: UpdateHost): Future[Resource] = {

    val resource = Resource(s"${lwmNamespace}users/${user.id}")

    val p = Promise[Resource]()

    blocking {
      s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |prefix owl: <http://www.w3.org/2002/07/owl#>
      |prefix foaf: <http://xmlns.com/foaf/0.1/>
      |prefix nco: <http://www.semanticdesktop.org/ontologies/nco#>
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

  def delete(resource: Resource): Future[Resource] = {
    import utils.Global._
    val p = Promise[Resource]()
    val individual = Individual(resource)
    if (individual.props(rdf.typ).contains(lwm.User)) {
      sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(resource)).map { b ⇒ p.success(resource) }
    }
    p.future
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
         |select ?s (${rdf.typ} as ?p) (${lwm.User} as ?o) where {
         | ?s ${rdf.typ} ${lwm.User} .
         | optional {?s ${foaf.lastName} ?lastname}
         |}order by desc(?lastname)
       """.stripMargin.execSelect().map { solution ⇒
      Resource(solution.data("s").toString)
    }
  }

  def substituteUserMapping(userId: String) = {
    import utils.Global._
    val query =
      s"""
          |select (?user as ?s) (${rdfs.label} as ?p) (?name as ?o) where {
          |  ?user ${rdf.typ} ${lwm.User} .
          |  ?user ${rdfs.label} ?name .
          |  filter not exists {?user ${lwm.hasGmId} "$userId"}
          |}
        """.stripMargin

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
      statement.s.toString.replaceAll("<", "").replaceAll(">", "") -> statement.o.toString
    }
  }

  def userFormMapping() = {
    import utils.Global._
    val query =
      s"""
          |select (?user as ?s) (${rdfs.label} as ?p) (?name as ?o) where {
          |  ?user ${rdf.typ} ${lwm.User} .
          |  ?user ${rdfs.label} ?name
          |}
        """.stripMargin

    SPARQLTools.statementsFromString(sparqlExecutionContext.executeQueryBlocking(query)).map { statement ⇒
      statement.s.toString.replaceAll("<", "").replaceAll(">", "") -> statement.o.toString
    }
  }

  def size()(implicit queryHost: QueryHost): Int = {
    import utils.Implicits._
    """
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |
      |select (count(distinct ?user) as ?count) {
      |   ?user rdf:type lwm:User
      |}
    """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }
}