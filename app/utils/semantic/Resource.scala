package utils.semantic

import java.net.{ URLDecoder, URLEncoder }
import java.util.UUID
import com.hp.hpl.jena.rdf.model.{ ResourceFactory, AnonId }
import org.joda.time.DateTime
import scala.xml.XML._

object ResourceUtils {
  def createResource(ns: Namespace) = Resource(s"$ns${AnonId.create()}")
  def createResource(ns: Namespace, id: UUID) = Resource(s"$ns${id.toString}")

}

trait RDFNode {
  val value: String
  def toQueryString: String
  def asResource(): Option[Resource]

  def asLiteral(): Option[Literal]
}

case class Namespace(uri: String) {
  override def toString = s"$uri"
}

case class Prefix(value: String)

case class Resource(value: String) extends RDFNode {
  override def toString = s"<$value>"

  override def asResource(): Option[Resource] = Some(this)

  override def asLiteral(): Option[Literal] = None

  override def toQueryString: String = toString
}

case class Property(value: String) extends RDFNode {
  override def toString = s"<$value>"

  override def asResource(): Option[Resource] = Some(Resource(value))

  override def asLiteral(): Option[Literal] = None

  override def toQueryString: String = toString
}

trait Literal extends RDFNode {
  val encoded: Boolean
  lazy val encodedString = if (encoded) value else URLEncoder.encode(value, "UTF-8")
  lazy val decodedString = if (encoded) URLDecoder.decode(value, "UTF-8") else value
}

case class StringLiteral(value: String, encoded: Boolean = false) extends Literal {

  override def toString = decodedString

  override def asResource(): Option[Resource] = None

  override def asLiteral(): Option[Literal] = Some(this)

  override def toQueryString: String = if (value == null) s"""""""" else s""""$encodedString""""
}

case class DateTimeLiteral(dateTime: DateTime, encoded: Boolean = false) extends Literal {
  override val value: String = dateTime.toString("yyyy-MM-dd")

  override def asResource(): Option[Resource] = None

  override def asLiteral(): Option[DateTimeLiteral] = Some(this)

  override def toQueryString: String = if (value == null) s"""""""" else s""""$decodedString""""
}

case class NamedGraph(uri: String) {
  override def toString = s"<$uri>"
}

case class Statement(s: Resource, p: Property, o: RDFNode) {
  override def toString: String = s"$s $p ${o.toQueryString}"
}

case class Individual(uri: Resource)(implicit executionContext: SPARQLExecution) {

  lazy val statements = properties

  lazy val props: Map[Property, List[RDFNode]] = {
    val statements = properties
    val g = statements.map(s ⇒ s.p -> s.o).groupBy(_._1)
    val l = g.map {
      e ⇒
        e._1 -> e._2.map(_._2)
    }
    l
  }

  /**
    * Lists all properties of this individual.
    * @return the list of known properties in the union graph
    */
  def properties: List[Statement] = {
    val response = executionContext.executeQueryBlocking(SPARQLBuilder.listIndividualProperties(uri))
    val xml = loadString(response)
    val t = SPARQLTools.statementsFromXML(xml)

    t.toList
  }

  /**
    * Adds a new statement to the named graph.
    * @param p the predicate
    * @param o the object
    * @return true, if the update of the named graph was successful
    */
  def add(p: Property, o: RDFNode): Boolean = {
    executionContext.executeUpdateBlocking(SPARQLBuilder.insertStatements(Statement(uri, p, o)))
  }

  def remove(property: Property, value: RDFNode): Boolean = {
    executionContext.executeUpdateBlocking(SPARQLBuilder.removeStatements(Statement(uri, property, value)))
  }

  def update(property: Property, oldValue: RDFNode, newValue: RDFNode): Boolean = {
    remove(property, oldValue)
    add(property, newValue)
  }

  /**
    * Checks whether the statement exists in the union graph.
    * @param p the predicate
    * @param o the object
    * @return true, if the statement exists
    */
  def exists(p: Property, o: RDFNode): Boolean = {
    executionContext.executeQueryBlocking(SPARQLBuilder.exists(Statement(uri, p, o))).contains("true")
  }

  def ontClasses: List[Resource] = {
    val response = executionContext.executeQueryBlocking(SPARQLBuilder.listIndividualProperties(uri, Vocabulary.RDF.typ))
    val xml = loadString(response)

    val statements = SPARQLTools.statementsFromXML(xml)
    statements.map(_.o.asResource().get).toList
  }

}