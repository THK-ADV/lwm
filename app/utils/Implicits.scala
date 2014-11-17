package utils

import com.hp.hpl.jena.query.QueryExecutionFactory
import com.hp.hpl.jena.rdf.model.RDFNode

case class QueryHost(host: String)

case class QuerySolution(data: Map[String, RDFNode])

object Implicits {

  implicit class StringWithSelectExecution(string: String) {

    import scala.collection.JavaConverters._

    def execSelect()(implicit queryHost: QueryHost): List[QuerySolution] = {
      val results = QueryExecutionFactory.sparqlService(queryHost.host, string).execSelect().asScala
      results.map { solution ⇒
        val data = solution.varNames().asScala.map { variable ⇒
          val r = solution.get(variable)
          variable -> r
        }
        QuerySolution(data.toMap)
      }.toList
    }
  }

}
