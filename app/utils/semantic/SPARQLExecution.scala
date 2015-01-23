package utils.semantic

import java.net.URLEncoder

import akka.util.Timeout

import scala.concurrent.Future
import scalaj.http.{ Http, HttpOptions }

object SPARQLExecution {
  def apply(queryHost: String, updateHost: String)(implicit timeout: Timeout) = new SPARQLExecution(updateHost, queryHost)
}

class SPARQLExecution(val updateHost: String, queryHost: String)(implicit timeout: Timeout) {
  import scala.concurrent.ExecutionContext.Implicits.global

  def executeUpdate(update: String): Future[Boolean] = Future {
    val updateEncoded = URLEncoder.encode(update, "UTF-8")
    Http.postData(updateHost, s"update=$updateEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("content-type", "application/x-www-form-urlencoded")
      .asString.toLowerCase
      .contains("success")
  }

  def executeQuery(query: String): Future[String] = Future {
    // println(s"Executing Query: \n $query")
    val start = System.nanoTime()
    val queryEncoded = URLEncoder.encode(query, "UTF-8")
    val response = Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString
    // println(s"Duration: ${(System.nanoTime() - start) / 1000000}")
    response
  }

  def executeBooleanQuery(query: String): Future[Boolean] = Future {
    val queryEncoded = URLEncoder.encode(query, "UTF-8")

    Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString.toLowerCase.contains("true")
  }

  def executeUpdateBlocking(update: String): Boolean = {
    val updateEncoded = URLEncoder.encode(update, "UTF-8")
    Http.postData(updateHost, s"update=$updateEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("content-type", "application/x-www-form-urlencoded")
      .asString.toLowerCase
      .contains("success")
  }

  def executeQueryBlocking(query: String): String = {
    val queryEncoded = URLEncoder.encode(query, "UTF-8")
    Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString
  }

  def executeBooleanQueryBlocking(query: String): Boolean = {
    val queryEncoded = URLEncoder.encode(query, "UTF-8")

    Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString.toLowerCase.contains("true")
  }
}
