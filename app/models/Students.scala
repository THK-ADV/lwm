package models

import java.net.{ URLEncoder, URLDecoder }

import com.hp.hpl.jena.query.QueryExecutionFactory
import org.joda.time.LocalDate
import utils.{ UpdateHost, QueryHost }
import utils.semantic._

import scala.concurrent.{ Promise, Future, blocking }

case class Student(
  gmId: String,
  firstname: String, lastname: String,
  registrationNumber: String,
  email: String,
  phone: String, degree: String)

object Students extends CheckedDelete {

  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.Implicits._

  def create(student: Student)(implicit updateHost: UpdateHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}students/${student.gmId}")

    val p = Promise[Resource]()
    blocking {
      s"""
     |${Vocabulary.defaulPrefixes}
     |
     |Insert data {
     |
     |    $resource rdf:type lwm:Student .
     |    $resource lwm:hasGmId "${student.gmId}" .
     |    $resource lwm:hasEnrollment <${student.degree}> .
     |    $resource lwm:hasRegistrationId "${student.registrationNumber}" .
     |    $resource foaf:firstName "${student.firstname}" .
     |    $resource foaf:lastName "${student.lastname}" .
     |    $resource foaf:mbox "${student.email}" .
     |    $resource nco:phoneNumber "${student.phone}" .
     |    $resource rdfs:label "${student.firstname} ${student.lastname}" .
     |
     |}
   """.stripMargin.execUpdate()
      p.success(resource)
    }
    p.future
  }

  def delete(gmId: String)(implicit updateHost: UpdateHost, queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}students/$gmId")
    delete(resource)
  }

  def all()(implicit queryHost: QueryHost): Future[List[Resource]] = Future {
    s"""
         |${Vocabulary.defaulPrefixes}
         |
         |select ?s (rdf:type as ?p) (lwm:Student as ?o) where {
         | ?s rdf:type lwm:Student .
         | optional {?s foaf:lastName ?lastname}
         |} order by asc(?lastname)
         |
       """.stripMargin.execSelect().map(s ⇒ Resource(s.data("s").toString))
  }

  def get(gmId: String)(implicit queryHost: QueryHost): Future[Resource] = {
    val resource = Resource(s"${utils.Global.lwmNamespace}students/$gmId")

    val p = Promise[Resource]()

    if (exists(gmId)) {
      p.success(resource)
    } else {
      p.failure(new NoSuchElementException(s"There is no student with ID $gmId"))
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

  override def check(resource: Resource)(implicit queryHost: QueryHost): Boolean = {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |ASK {
       |  $resource rdf:type lwm:Student
       |}
     """.stripMargin.executeAsk()
  }

  def size()(implicit queryHost: QueryHost): Int = {
    s"""
       |${Vocabulary.defaulPrefixes}
       |
       |Select (count(distinct ?s) as ?count) where {
       |  ?s rdf:type lwm:Student
       |}
     """.stripMargin.execSelect().head.data("count").asLiteral().getInt
  }

  //------------------ UNREFACTORED ---------------

  private def search(inputQuery: String): Future[List[(String, String, String)]] = {
    import utils.Global._
    import utils.semantic.Vocabulary._

    val sparqlQuery =
      s"""
        |SELECT ?s (${lwm.hasGmId} as ?p) ?o where {?s ${lwm.hasGmId} ?o
        |FILTER regex(?o, "^$inputQuery")
        |}
      """.stripMargin

    val r = for {
      result ← sparqlExecutionContext.executeQuery(sparqlQuery)
    } yield {
      val statements = SPARQLTools.statementsFromString(result)
      statements.map { statement ⇒
        val individual = Individual(statement.s)
        val name = individual.props.getOrElse(rdfs.label, List(StringLiteral(""))).head.value
        (statement.o.value, name, statement.s.value)
      }
    }
    r.map(_.toList)
  }

  def search(query: String, maxCount: Int): Future[List[(String, String, String)]] = if (maxCount > 0) search(query).map(_.sortBy(_._1).take(maxCount)) else search(query).map(_.sortBy(_._1))

  def labworksForStudent(student: Resource) = {
    import utils.semantic.Vocabulary._
    import utils.Global._
    val query1 =
      s"""
         |select * where {
         | $student ${lwm.memberOf} ?group .
         | ?group ${lwm.hasLabWork} ?labwork.
         | ?labwork ${rdfs.label} ?labworkName .
         |}order by desc(?labworkName)
       """.stripMargin
    val results = QueryExecutionFactory.sparqlService(queryHost, query1).execSelect()
    var mapping = List.empty[(Resource, String)]
    while (results.hasNext) {
      val solution = results.nextSolution()
      val labworkResource = solution.getResource("labwork")
      val name = if (solution.getLiteral("labworkName") == null) "" else URLDecoder.decode(solution.getLiteral("labworkName").getString, "UTF-8")
      if (labworkResource != null) {
        mapping = (Resource(labworkResource.getURI), name) :: mapping
      }
    }
    mapping
  }

  def dateCountMissed(student: Resource, group: Resource)(implicit queryHost: QueryHost): Int = {
    import utils.Implicits._
    val q = s"""
        prefix lwm: <http://lwm.gm.fh-koeln.de/>

        select (count(?attended) as ?count) where {
          $student lwm:hasScheduleAssociation ?association .
          ?association lwm:hasAssignmentDate ?date .
          ?association lwm:hasGroup $group .
          optional{?association lwm:hasAttended ?attended} .
          optional{
              ?association lwm:hasAlternateScheduleAssociation ?alternate .
              ?alternate lwm:hasAssignmentDate ?alternateDate .
            } .
          filter(?date < "${LocalDate.now().toString("yyyy-MM-dd")}")
          filter(?attended = "false")
          optional{filter(?alternateDate < "${LocalDate.now().toString("yyyy-MM-dd")}")}
        }
     """.stripMargin

    q.execSelect().headOption.map { solution ⇒
      solution.data.get("count").map(_.asLiteral().getInt)
    }.flatten.getOrElse(0)
  }

  def dateCountNotPassed(student: Resource, group: Resource)(implicit queryHost: QueryHost): Int = {

    s"""
       prefix lwm: <http://lwm.gm.fh-koeln.de/>

       select (count(?notPassed) as ?count) where {
         {
         ### not passed dates
           select ?notPassed where {
              $student lwm:hasScheduleAssociation ?association .
              ?association lwm:hasGroup $group .
              ?association lwm:hasAssignmentDate ?date .

              optional{?association lwm:hasPassed ?notPassed} .
              filter(?date < "${LocalDate.now().toString("yyyy-MM-dd")}") .
              filter(?notPassed = "false") .
              filter not exists {?association lwm:hasAlternateScheduleAssociation ?alternate } .
            }
         } union {
         ### Alternate dates that are not passed
           select ?notPassed where {
                  $student lwm:hasScheduleAssociation ?association .
                  ?association lwm:hasGroup $group .
                  ?association lwm:hasAlternateScheduleAssociation ?alternate .
                  ?alternate lwm:hasAssignmentDate ?alternatedate .
                  optional{?association lwm:hasPassed ?notPassed} .

                  filter(?notPassed = "false") .
                  filter exists {
                     ?association lwm:hasAlternateScheduleAssociation ?alternate .
                     ?alternate lwm:hasAssignmentDate ?alternatedate .
                  } .
                  filter(?alternatedate < "${LocalDate.now().toString("yyyy-MM-dd")}") .
           }
           }
         }

     """.stripMargin.execSelect().headOption.map { solution ⇒
      solution.data.get("count").map(_.asLiteral().getInt)
    }.flatten.getOrElse(0)
  }

  def dateCountAlternate(student: Resource, group: Resource)(implicit queryHost: QueryHost): Int = {

    s"""
        prefix lwm: <http://lwm.gm.fh-koeln.de/>

        select (count(?alternate) as ?count) where {
          $student lwm:hasScheduleAssociation ?association .
          ?association lwm:hasAlternateScheduleAssociation ?alternate .
          ?association lwm:hasGroup $group .
          ?association lwm:hasAssignmentDate ?date .
          filter(?date < "${LocalDate.now().toString("yyyy-MM-dd")}")
        }
     """.stripMargin.execSelect().headOption.map { solution ⇒
      solution.data.get("count").map(_.asLiteral().getInt)
    }.flatten.getOrElse(0)
  }

  def studentForLabworkAssociation(labworkAssociation: Resource)(implicit queryHost: QueryHost) = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |select ?student ?id where {
      |    ?student lwm:hasScheduleAssociation $labworkAssociation .
      |    ?student rdf:type lwm:Student .
      |    ?student lwm:hasGmId ?id .
      |}
    """.stripMargin.execSelect().map { solution ⇒
      solution.data("id").asLiteral().getString
    }.take(1)
  }

  def isHidden(labwork: Resource, student: Resource)(implicit queryHost: QueryHost) = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |select (count(?state) as ?count) where {
      |    $student lwm:hasHidingState ?state .
      |    ?state lwm:hasHidingSubject $labwork .
      |}
    """.stripMargin.execSelect().map { solution ⇒
      solution.data("count").asLiteral().getInt > 0
    }.head
  }

  def removeHideState(labwork: Resource, student: Resource)(implicit updateHost: UpdateHost) = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |delete {
      |  ?state ?p ?o .
      |  $student lwm:hasHidingState ?state
      |} where {
      |  select * where{
      |    $student lwm:hasHidingState ?state .
      |    ?state lwm:hasHidingSubject $labwork
      |  }
      |}
    """.stripMargin.execUpdate()
  }

  def addHideState(labwork: Resource, student: Resource)(implicit updateHost: UpdateHost) = {
    s"""
      |prefix lwm: <http://lwm.gm.fh-koeln.de/>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |insert data {
      | $student lwm:hasHidingState [
      |   lwm:hasHidingSubject $labwork
      | ]
      |}
    """.stripMargin.execUpdate()
  }

}

object StudentForms {

  import play.api.data.Forms._
  import play.api.data._

}

