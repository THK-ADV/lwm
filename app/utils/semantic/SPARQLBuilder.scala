package utils.semantic

object SPARQLBuilder {

  private val rdf = Vocabulary.RDF
  private val rdfs = Vocabulary.RDFS
  private val owl = Vocabulary.OWL

  def createClass(uri: Resource, graph: NamedGraph): String = {
    s"INSERT DATA {GRAPH $graph {$uri ${rdf.typ} ${owl.Class}}}"
  }

  def createIndividual(uri: Resource): String = s"INSERT DATA {$uri ${rdf.typ} ${owl.NamedIndividual}}"

  def listIndividualProperties(uri: Resource): String =
    s"SELECT ($uri AS ?s) ?p ?o WHERE {$uri ?p ?o}"

  def listIndividualProperties(uri: Resource, property: Property): String = s"SELECT ($uri AS ?s) ($property as ?p) ?o {$uri $property ?o}"

  def listIndividuals() =
    s"SELECT ?s (${rdf.typ} as ?p) (${owl.NamedIndividual} as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual}}"

  def listIndividualsWithClass(uri: Resource) =
    s"SELECT ?s (${rdf.typ} as ?p) ($uri as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s ${rdf.typ} $uri}"

  def listIndividualsWithProperty(p: Property, value: StringLiteral) =
    s"""SELECT ?s ($p as ?p) (${value.toQueryString} as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p ${value.toQueryString}}"""

  def listIndividualsWithResource(p: Property, value: Resource) =
    s"SELECT ?s ($p as ?p) ($value as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p $value}"

  def listStatements(): String = s"SELECT * WHERE {?s ?p ?o}"

  def insertStatements(statements: Statement*): String =
    s"INSERT DATA ${statements.mkString("{", ".", "}")}"

  def removeIndividual(uri: Resource) =
    s"DELETE {$uri ?p ?o} WHERE {$uri ?p ?o} ; DELETE {?s ?p $uri} WHERE {?s ?p $uri}"

  def removeStatements(statements: Statement*) =
    s"DELETE DATA ${statements.mkString("{", ".", "}")}"

  def exists(uri: Resource) = s"ASK {$uri ?p ?o}"

  def exists(stmt: Statement) = s"ASK {${stmt.s} ${stmt.p} ${stmt.o}}"

  def listIndividualsWithClassAndProperty(cls: Resource, prop: Property, value: RDFNode) =
    s"""
       |SELECT ?s (${rdf.typ} as ?p) ($cls as ?o) WHERE {
       |?s ${rdf.typ} ${owl.NamedIndividual} .
       |?s ${rdf.typ} $cls .
       |?s $prop ${value.toQueryString}
       |}
       |""".stripMargin
}
