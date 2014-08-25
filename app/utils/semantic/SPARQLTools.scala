package utils.semantic

import java.net.URLDecoder

import scala.xml.Elem

object SPARQLTools {
  def statementsFromString(s: String) = {
    import scala.xml.XML._
    val xml = loadString(s)
    statementsFromXML(xml)
  }
  def statementsFromXML(xml: Elem) = {

    for {
      x ← xml \\ "result"
    } yield {
      var l = false
      val res = (for (b ← x \ "binding") yield {
        val name = (b \ "@name").text
        val uri = b \ "uri"
        val literal = b \ "literal"
        val value = if (uri.nonEmpty) uri.text
        else {
          l = true; URLDecoder.decode(literal.text, "UTF-8")
        }
        name -> value
      }).toMap

      if (l) {
        Statement(Resource(res("s")), Property(res("p")), Literal(res("o")))
      } else {
        Statement(Resource(res("s")), Property(res("p")), Resource(res("o")))
      }
    }
  }
}
