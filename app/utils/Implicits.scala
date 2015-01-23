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
      val start = System.nanoTime()
      val results = QueryExecutionFactory.sparqlService(queryHost.host, string).execSelect().asScala
      val m = results.map { solution ⇒
        val data = solution.varNames().asScala.map { variable ⇒
          val r = solution.get(variable)
          variable -> r
        }
        QuerySolution(data.toMap)
      }.toList
      println(s"Select Duration: ${(System.nanoTime() - start) / 1000000}")
      m
    }

    def execUpdate()(implicit updateHost: UpdateHost) = {
      val request = UpdateFactory.create(string)
      UpdateExecutionFactory.createRemote(request, updateHost.host).execute()
    }

    def executeAsk()(implicit queryHost: QueryHost) = {
      println(string)
      val start = System.nanoTime()
      val r = QueryExecutionFactory.sparqlService(queryHost.host, string).execAsk()
      println(s"ASK Duration: ${(System.nanoTime() - start) / 1000000}")
      r
    }
  }

}
