package utils.semantic

import java.net.{URLDecoder, URLEncoder}
import com.hp.hpl.jena.rdf.model.AnonId
import scala.xml.XML._

object ResourceUtils{
  def createResource(ns: Namespace) = Resource(s"$ns${AnonId.create()}")
}

trait RDFNode {
  val value: String
  def toQueryString: String
  def asResource(): Option[Resource]

  def asLiteral(): Option[Literal]
}

case class Namespace(uri: String){
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

case class Literal(value: String, encoded: Boolean = false) extends RDFNode {
  val encodedString = if (encoded) value else URLEncoder.encode(value, "UTF-8")
  val decodedString = if (encoded) URLDecoder.decode(value, "UTF-8") else value

  override def toString = decodedString

  override def asResource(): Option[Resource] = None

  override def asLiteral(): Option[Literal] = Some(this)

  override def toQueryString: String = if (value == null) s"""""""" else s""""$encodedString""""
}

case class NamedGraph(uri: String) {
  override def toString = s"<$uri>"
}

case class Statement(s: Resource, p: Property, o: RDFNode) {
  override def toString: String = s"$s $p ${o.toQueryString}"
}

case class Individual(uri: Resource)(implicit executionContext: SPARQLExecution) {

  lazy val statements = properties

  lazy val props: Map[Property, RDFNode] = {
    val statements = properties
    statements.map(s => s.p -> s.o).toMap.withDefaultValue(Literal(""))
  }

  /**
   * Lists all properties of this individual.
   * @return the list of known properties in the union graph
   */
  def properties: List[Statement] = {
    val response = executionContext.executeQuery(SPARQLBuilder.listIndividualProperties(uri))
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
  def add(p: Property, o: RDFNode)(implicit graph: NamedGraph): Boolean = {
    executionContext.executeUpdate(SPARQLBuilder.insertStatements(graph, Statement(uri, p, o)))
  }

  def remove(property: Property, value: RDFNode)(implicit graph: NamedGraph): Boolean = {
    executionContext.executeUpdate(SPARQLBuilder.removeStatements(graph, Statement(uri, property, value)))
  }

  def update(property: Property, oldValue: RDFNode, newValue: RDFNode)(implicit graph: NamedGraph): Boolean = {
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
    executionContext.executeQuery(SPARQLBuilder.exists(Statement(uri, p, o))).contains("true")
  }

  def ontClasses: List[Resource] = {
    val response = executionContext.executeQuery(SPARQLBuilder.listIndividualProperties(uri, Vocabulary.RDF.typ))
    val xml = loadString(response)

    val statements = SPARQLTools.statementsFromXML(xml)
    statements.map(_.o.asResource().get).toList
  }

}