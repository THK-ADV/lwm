package utils.semantic

object SPARQLBuilder {

  private val rdf = Vocabulary.RDF
  private val rdfs = Vocabulary.RDFS
  private val owl = Vocabulary.OWL

  def createClass(uri: Resource, graph: NamedGraph): String = {
    s"INSERT DATA {GRAPH $graph {$uri ${rdf.typ} ${owl.Class}}}"
  }

  def createIndividual(uri: Resource, graph: NamedGraph): String = s"INSERT DATA {GRAPH $graph {$uri ${rdf.typ} ${owl.NamedIndividual}}}"

  def createNamedGraph(uri: Resource) = s"CREATE GRAPH $uri"

  def listIndividualProperties(uri: Resource, graph: NamedGraph): String =
    s"SELECT ($uri AS ?s) ?p ?o FROM NAMED $graph WHERE {$uri ?p ?o}"

  def listIndividualProperties(uri: Resource): String =
    s"SELECT ($uri AS ?s) ?p ?o WHERE {$uri ?p ?o}"

  def listIndividualProperties(uri: Resource, property: Property): String = s"SELECT ($uri AS ?s) ($property as ?p) ?o {$uri $property ?o}"

  def listIndividuals(graph: NamedGraph) =
    s"SELECT ?s (${rdf.typ} as ?p) (<${owl.NamedIndividual}> as ?o) FROM NAMED $graph WHERE {?s ${rdf.typ} ${owl.NamedIndividual}}"

  def listIndividuals() =
    s"SELECT ?s (${rdf.typ} as ?p) (${owl.NamedIndividual} as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual}}"

  def listIndividualsWithClass(uri: Resource, graph: NamedGraph) =
    s"SELECT ?s (${rdf.typ} as ?p) ($uri as ?o) FROM NAMED $graph WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s ${rdf.typ} $uri}"

  def listIndividualsWithClass(uri: Resource) =
    s"SELECT ?s (${rdf.typ} as ?p) ($uri as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s ${rdf.typ} $uri}"

  def listIndividualsWithProperty(p: Property, value: Literal, graph: NamedGraph) =
    s"SELECT ?s ($p as ?p) ($value as ?o) FROM NAMED $graph WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p ${value.toQueryString}}"

  def listIndividualsWithProperty(p: Property, value: Literal) =
    s"""SELECT ?s ($p as ?p) ($value as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p ${value.toQueryString}"""

  def listIndividualsWithResource(p: Property, value: Resource, graph: NamedGraph) =
    s"SELECT ?s ($p as ?p) ($value as ?o) FROM NAMED $graph WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p $value}"

  def listIndividualsWithResource(p: Property, value: Resource) =
    s"SELECT ?s ($p as ?p) ($value as ?o) WHERE {?s ${rdf.typ} ${owl.NamedIndividual} . ?s $p $value}"

  def listStatements(graph: NamedGraph): String = s"SELECT * FROM NAMED $graph WHERE {?s ?p ?o}"

  def listStatements(): String = s"SELECT * WHERE {?s ?p ?o}"

  def insertStatements(graph: NamedGraph, statements: Statement*): String =
    s"INSERT DATA {GRAPH $graph ${statements.mkString("{", ".", "}")}}"

  def removeIndividual(uri: Resource, graph: NamedGraph) =
    s"WITH $graph DELETE {$uri ?p ?o} WHERE {$uri ?p ?o} ; WITH $graph DELETE {?s ?p $uri} WHERE {?s ?p $uri}"

  def removeStatements(graph: NamedGraph, statements: Statement*) =
    s"DELETE DATA {GRAPH $graph ${statements.mkString("{", ".", "}")}}"

  def exists(uri: Resource, graph: NamedGraph) = s"ASK FROM NAMED $graph {$uri ?p ?o}"

  def exists(uri: Resource) = s"ASK {$uri ?p ?o}"

  def exists(stmt: Statement, graph: NamedGraph) = s"ASK FROM NAMED $graph {${stmt.s} ${stmt.p} ${stmt.o}}"

  def exists(stmt: Statement) = s"ASK {${stmt.s} ${stmt.p} ${stmt.o}}"

  def loadGraph(resource: Resource, graph: NamedGraph): String = s"LOAD $resource INTO GRAPH $graph"

}
