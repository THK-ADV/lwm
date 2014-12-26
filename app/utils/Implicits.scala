package utils

import com.hp.hpl.jena.query.QueryExecutionFactory
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.update.{ UpdateFactory, UpdateExecutionFactory }

case class QueryHost(host: String)

case class UpdateHost(host: String)

case class QuerySolution(data: Map[String, RDFNode])

object Implicits {

  implicit class StringWithSelectExecution(string: String) {

    import scala.collection.JavaConverters._

    def execSelect()(implicit queryHost: QueryHost): List[QuerySolution] = {
      println(string)
      val results = QueryExecutionFactory.sparqlService(queryHost.host, string).execSelect().asScala
      results.map { solution ⇒
        val data = solution.varNames().asScala.map { variable ⇒
          val r = solution.get(variable)
          variable -> r
        }
        QuerySolution(data.toMap)
      }.toList
    }

    def execUpdate()(implicit updateHost: UpdateHost) = {
      val request = UpdateFactory.create(string)
      UpdateExecutionFactory.createRemote(request, updateHost.host).execute()
    }
  }

}
