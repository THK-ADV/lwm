package utils.semantic

import java.net.URLEncoder

import akka.util.Timeout

import scalaj.http.{Http, HttpOptions}

object SPARQLExecution {
  def executeUpdate(host: String, query: String): Boolean = {
    Http.post(host)
      .param("update", query)
      .option(HttpOptions.readTimeout(5000))
      .asString.contains("Success")
  }

  def executeSelect(host: String, query: String): String = {
    Http(host)
      .param("query", query)
      .option(HttpOptions.readTimeout(5000))
      .header("accept", "text/xml")
      .asString
  }

  def apply(queryHost: String, updateHost: String)(implicit timeout: Timeout) = new SPARQLExecution(updateHost, queryHost)
}

class SPARQLExecution(val updateHost: String, queryHost: String)(implicit timeout: Timeout) {

  def executeUpdate(update: String): Boolean = {
    val updateEncoded = URLEncoder.encode(update, "UTF-8")
    Http.postData(updateHost, s"update=$updateEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("content-type", "application/x-www-form-urlencoded")
      .asString.toLowerCase
      .contains("success")
  }

  def executeQuery(query: String): String = {
    val queryEncoded = URLEncoder.encode(query, "UTF-8")
    Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString
  }

  def executeBooleanQuery(query: String) = {
    println(query)
    val queryEncoded = URLEncoder.encode(query, "UTF-8")

    Http.postData(queryHost, s"query=$queryEncoded")
      .option(HttpOptions.readTimeout(timeout.duration.toMillis.toInt))
      .header("accept", "text/xml")
      .asString.toLowerCase.contains("true")
  }
}
