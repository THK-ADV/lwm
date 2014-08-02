package utils.semantic

object Vocabulary {

  trait ResourceHelper {
    val ns: Namespace
    val prefix: Prefix

    def resource(localName: String) = Resource(s"$ns$localName")
    def property(localName: String) = Property(s"$ns$localName")
  }

  object LWM extends ResourceHelper{
    val ns = Namespace("http://lwm.gm.fh-koeln.de/ns/")
    val prefix = Prefix("lwm")

    val Student = resource("Student")
    val User = resource("User")
    val Degree = resource("Degree")
    val AutomationITMaster = resource("AutomationITMaster")
    val BusinessAdministrationAndEngineeringBachelor = resource("BusinessAdministrationAndEngineeringBachelor")
    val BusinessInformationSystemsBachelor = resource("BusinessInformationSystemsBachelor")
    val BusinessInformationSystemsMaster = resource("BusinessInformationSystemsMaster")
    val BusinessInformationSystemsBachelorJoint = resource("BusinessInformationSystemsBachelorJoint")
    val BusinessInformationSystemsMasterJoint = resource("BusinessInformationSystemsMasterJoint")
    val ComputerEngineeringBachelor = resource("ComputerEngineeringBachelor")
    val ComputerScienceBachelor = resource("ComputerScienceBachelor")
    val ComputerScienceMaster = resource("ComputerScienceMaster")
    val ElectricalEngineeringBachelor = resource("ElectricalEngineeringBachelor")
    val GeneralMechanicalEngineeringBachelor = resource("GeneralMechanicalEngineeringBachelor")
    val MediaInformaticsBachelor = resource("MediaInformaticsBachelor")
    val MediaInformaticsMaster = resource("MediaInformaticsMaster")
    val ProductDesignAndProcessDevelopmentMaster = resource("ProductDesignAndProcessDevelopmentMaster")
    val WebScienceMaster = resource("WebScienceMaster")


    val registrationId = property("hasRegistrationId")
    val systemId = property("hasSystemId")
    val isSystemIdFor = property("isSystemIdFor")
    val isRegistrationIdFor = property("isRegistrationIdFor")

    val hasEnrollment = property("hasEnrollment")


  }

  /**
    * XML Schema
    */
  object XSD extends ResourceHelper {
    val ns = Namespace("http://www.w3.org/2001/XMLSchema#")
    val prefix = Prefix("xsd")
  }

  /**
    * RDF Base Vocabulary
    */
  object RDF extends ResourceHelper {
    val ns = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    val prefix = Prefix("rdf")

    val Alt = resource("Alt")
    val Bag = resource("Bag")
    val Property = resource("Property")
    val Seq = resource("Seq")
    val Statement = resource("Statement")
    val List = resource("List")
    val nil = resource("nil")

    val first = property("first")
    val rest = property("rest")
    val subject = property("subject")
    val predicate = property("predicate")
    val obj = property("object")
    val typ = property("type")
    val value = property("value")
  }

  /**
    * RDF Schema Vocabulary
    */
  object RDFS extends ResourceHelper {
    val ns = Namespace("http://www.w3.org/2000/01/rdf-schema#")
    val prefix = Prefix("rdfs")

    val Class = resource("Class")
    val Datatype = resource("Datatype")
    val Container = resource("Container")
    val ContainerMembershipProperty = resource("ContainerMembershipProperty")
    val Literal = resource("Literal")
    val Ressource = resource("Ressource")

    val comment = property("comment")
    val domain = property("domain")
    val label = property("label")
    val isDefinedBy = property("isDefinedBy")
    val range = property("range")
    val seeAlso = property("seeAlso")
    val subClassOf = property("subClassOf")
    val member = property("member")
  }

  /**
    * OWL Base Vocabulary
    */
  object OWL extends ResourceHelper {
    val ns = Namespace("http://www.w3.org/2002/07/owl#")
    val prefix = Prefix("owl")

    val AllDifferent = resource("AllDifferent")
    val AllDisjointClasses = resource("AllDisjointClasses")
    val AllDisjointProperties = resource("AllDisjointProperties")
    val Annotation = resource("Annotation")
    val AnnotationProperty = resource("AnnotationProperty")
    val AsymmetricProperty = resource("AsymmetricProperty")
    val Axiom = resource("Axiom")
    val Class = resource("Class")
    val DataRange = resource("DataRange")
    val DatatypeProperty = resource("DatatypeProperty")
    val DeprecatedClass = resource("DeprecatedClass")
    val DeprecatedProperty = resource("DeprecatedProperty")
    val FunctionalProperty = resource("FunctionalProperty")
    val IrreflexiveProperty = resource("IrreflexiveProperty")
    val NamedIndividual = resource("NamedIndividual")
    val NegativePropertyAssertion = resource("NegativePropertyAssertion")
    val Nothing = resource("Nothing")
    val ObjectProperty = resource("ObjectProperty")
    val Ontology = resource("Ontology")
    val OntologyProperty = resource("OntologyProperty")
    val ReflexiveProperty = resource("ReflexiveProperty")
    val Restriction = resource("Restriction")
    val SymmetricProperty = resource("SymmetricProperty")
    val Thing = resource("Thing")
    val TransitiveProperty = resource("TransitiveProperty")

    val allValuesFrom = property("allValuesFrom")
    val annotatedProperty = property("annotatedProperty")
    val annotatedSource = property("annotatedSource")
    val annotatedTarget = property("annotatedTarget")
    val assertionProperty = property("assertionProperty")
    val backwardCompatibleWith = property("backwardCompatibleWith")
    val bottomDataProperty = property("bottomDataProperty")
    val cardinality = property("cardinality")
    val complementOf = property("complementOf")
    val datatypeComplementOf = property("datatypeComplementOf")
    val deprecated = property("deprecated")
    val differentFrom = property("differentFrom")
    val disjointUnionOf = property("disjointUnionOf")
    val disjointWith = property("disjointWith")
    val equivalentClass = property("equivalentClass")
    val equivalentProperty = property("equivalentProperty")
    val hasKey = property("hasKey")
    val hasSelf = property("hasSelf")
    val hasValue = property("hasValue")
    val imports = property("imports")
    val incompatibleWith = property("incompatibleWith")
    val intersectionOf = property("intersectionOf")
    val inverseOf = property("inverseOf")
    val maxQualifiedCardinality = property("maxQualifiedCardinality")
    val minCardinality = property("minCardinality")
    val onClass = property("onClass")
    val onDataRange = property("onDataRange")
    val onDatatype = property("onDatatype")
    val onProperties = property("onProperties")
    val onProperty = property("onProperty")
    val oneOf = property("oneOf")
    val priorVersion = property("priorVersion")
    val propertyChainAxiom = property("propertyChainAxiom")
    val propertyDisjointWith = property("propertyDisjointWith")
    val qualifiedCardinality = property("qualifiedCardinality")
    val sameAs = property("sameAs")
    val someValuesFrom = property("someValuesFrom")
    val sourceIndividual = property("sourceIndividual")
    val targetIndividual = property("targetIndividual")
    val targetValue = property("targetValue")
    val topDataProperty = property("topDataProperty")
    val stopObjectPropertyameAs = property("topObjectProperty")
    val unionOf = property("unionOf")
    val versionIRI = property("versionIRI")
    val versionInfo = property("versionInfo")
    val withRestrictions = property("withRestrictions")
  }


  /**
    * Nepomuk Messaging Ontology.
    */
  object NMO extends ResourceHelper {
    val prefix = Prefix("nmo")
    val ns = Namespace("http://www.semanticdesktop.org/ontologies/nmo#")

    val Email = resource("Email")
    val IMMessage = resource("IMMessage")
    val Message = resource("Message")

    val bcc = property("bcc")
    val cc = property("cc")
    val contentMimeType = property("contentMimeType")
    val from = property("from")
    val hasAttachment = property("hasAttachment")
    val htmlMessageContent = property("htmlMessageContent")
    val inReplyTo = property("inReplyTo")
    val isRead = property("isRead")
    val messageSubject = property("messageSubject")
    val plainTextMessageContent = property("plainTextMessageContent")
    val primaryRecipient = property("primaryRecipient")
    val receivedDate = property("receivedDate")
    val recipient = property("recipient")
    val replyTo = property("replyTo")
    val secondaryRecipient = property("secondaryRecipient")
    val sender = property("sender")
    val sentDate = property("sentDate")
    val to = property("to")
  }

  /**
    * Nepomuk Communication Ontology.
    */
  object NCO extends ResourceHelper {
    val prefix = Prefix("nco")
    val ns = Namespace("http://www.semanticdesktop.org/ontologies/nco#")

    val Affiliation = resource("Affiliation")
    val AudioIMAccount = resource("AudioIMAccount")
    val BbsNumber = resource("BbsNumber")
    val CarPhoneNumber = resource("CarPhoneNumber")
    val CellPhoneNumber = resource("CellPhoneNumber")
    val Contact = resource("Contact")
    val ContactGroup = resource("ContactGroup")
    val ContactList = resource("ContactList")
    val ContactListDataObject = resource("ContactListDataObject")
    val ContactMedium = resource("ContactMedium")
    val DomesticDeliveryAddress = resource("DomesticDeliveryAddress")
    val EmailAddress = resource("EmailAddress")
    val Gender = resource("Gender")
    val IMAccount = resource("IMAccount")
    val InternationalDeliveryAddress = resource("InternationalDeliveryAddress")
    val IsdnNumber = resource("IsdnNumber")
    val MessagingNumber = resource("MessagingNumber")
    val ModemNumber = resource("ModemNumber")
    val OrganizationContact = resource("OrganizationContact")
    val PagerNumber = resource("PagerNumber")
    val ParcelDeliveryAddress = resource("ParcelDeliveryAddress")
    val PcsNumber = resource("PcsNumber")
    val PersonContact = resource("PersonContact")
    val PhoneNumber = resource("PhoneNumber")
    val PostalAddress = resource("PostalAddress")
    val Role = resource("Role")
    val VideoIMAccount = resource("VideoIMAccount")
    val VideoTelephoneNumber = resource("VideoTelephoneNumber")
    val VoicePhoneNumber = resource("VoicePhoneNumber")

    val addressLocation = property("addressLocation")
    val belongsToGroup = property("belongsToGroup")
    val birthDate = property("birthDate")
    val blogUrl = property("blogUrl")
    val contactGroupName = property("contactGroupName")
    val contactMediumComment = property("contactMediumComment")
    val contactUID = property("contactUID")
    val containsContact = property("containsContact")
    val contributor = property("contributor")
    val country = property("country")
    val creator = property("creator")
    val department = property("department")
    val emailAddress = property("emailAddress")
    val extendedAddress = property("extendedAddress")
    val foafUrl = property("foafUrl")
    val fullname = property("fullname")
    val gender = property("gender")
    val hasAffiliation = property("hasAffiliation")
    val hasContactMedium = property("hasContactMedium")
    val hasEmailAddress = property("hasEmailAddress")
    val hasIMAccount = property("hasIMAccount")
    val hasLocation = property("hasLocation")
    val hasPhoneNumber = property("hasPhoneNumber")
    val hasPostalAddress = property("hasPostalAddress")
    val hobby = property("hobby")
    val imAccountType = property("imAccountType")
    val imID = property("imID")
    val imNickname = property("imNickname")
    val imStatus = property("imStatus")
    val key = property("key")
    val locality = property("locality")
    val logo = property("logo")
    val nameAdditional = property("nameAdditional")
    val nameFamily = property("nameFamily")
    val nameGiven = property("nameGiven")
    val nickname = property("nickname")
    val note = property("note")
    val org = property("org")
    val phoneNumber = property("phoneNumber")
    val photo = property("photo")
    val pobox = property("pobox")
    val postalcode = property("postalcode")
    val publisher = property("publisher")
    val representative = property("representative")
    val role = property("role")
    val sound = property("sound")
    val streetAddress = property("streetAddress")
    val title = property("title")
    val url = property("url")
    val voiceMail = property("voiceMail")
    val websiteUrl = property("websiteUrl")
  }

  /**
    * Friend of a Friend Ontology.
    */
  object FOAF extends ResourceHelper {
    val prefix = Prefix("foaf")
    val ns = Namespace("http://xmlns.com/foaf/0.1/")

    /**
      * Class: foaf:Agent
      * Agent - An agent (eg. person, group, software or physical artifact).
      *
      * Status: 	stable
      *
      * Properties include:
      * weblog icqChatID msnChatID account age mbox yahooChatID tipjar jabberID status openid gender
      * interest holdsAccount topic_interest aimChatID birthday made skypeID mbox_sha1sum
      * Used with: 	member maker
      * Has Subclass 	Organization Group Person
      *
      * The Agent class is the class of agents; things that do stuff. A well known sub-class is Person,
      * representing people. Other kinds of agents include Organization and Group.
      *
      * The Agent class is useful in a few places in FOAF where Person would have been overly specific.
      * For example, the IM chat ID properties such as jabberID are typically associated with people,
      * but sometimes belong to software bots.
      */
    val Agent = resource("Agent")

    /**
      * Class: foaf:Group
      * Group - A class of Agents.
      * Status: 	stable
      * Properties include: 	member
      * Subclass Of 	Agent
      *
      * The Group class represents a collection of individual agents (and may itself play the role of a Agent, ie.
      * something that can perform actions).
      *
      * This concept is intentionally quite broad, covering informal and ad-hoc groups, long-lived communities,
      * organizational groups within a workplace, etc. Some such groups may have associated characteristics which could
      * be captured in RDF (perhaps a homepage, name, mailing list etc.).
      *
      * While a Group has the characteristics of a Agent, it is also associated with a number of other Agents
      * (typically people) who constitute the Group. FOAF provides a mechanism, the membershipClass property, which
      * relates a Group to a sub-class of the class Agent who are members of the group. This is a little complicated,
      * but allows us to make group membership rules explicit.
      *
      * The markup (shown below) for defining a group is both complex and powerful. It allows group membership rules
      * to match against any RDF-describable characteristics of the potential group members. As FOAF and similar
      * vocabularies become more expressive in their ability to describe individuals, the Group mechanism for
      * categorising them into groups also becomes more powerful.
      *
      * While the formal description of membership criteria for a Group may be complex, the basic mechanism for
      * saying that someone is in a Group is very simple. We simply use a member property of the Group to indicate
      * the agents that are members of the group. For example:
      *
      *
      * <foaf:Group>
      * <foaf:name>ILRT staff</foaf:name>
      * <foaf:member>
      * <foaf:Person>
      * <foaf:name>Martin Poulter</foaf:name>
      * <foaf:homepage rdf:resource="http://www.ilrt.bris.ac.uk/aboutus/staff/staffprofile/?search=plmlp"/>
      * <foaf:workplaceHomepage rdf:resource="http://www.ilrt.bris.ac.uk/"/>
      * </foaf:Person>
      * </foaf:member>
      * </foaf:Group>
      *
      * Behind the scenes, further RDF statements can be used to express the rules for being a member of this group.
      * End-users of FOAF need not pay attention to these details.
      *
      * Here is an example. We define a Group representing those people who are ILRT staff members
      * (ILRT is a department at the University of Bristol). The membershipClass property connects the group
      * (conceived of as a social entity and agent in its own right) with the class definition for those people who
      * constitute it. In this case, the rule is that all group members are in the ILRTStaffPerson class, which is in
      * turn populated by all those things that are a Person and which have a workplaceHomepage of http://www.ilrt.bris.ac.uk/.
      * This is typical: FOAF groups are created by specifying a sub-class of Agent (in fact usually this will be
      * a sub-class of Person), and giving criteria for which things fall in or out of the sub-class. For this,
      * we use the owl:onProperty and owl:hasValue properties, indicating the property/value pairs which must
      * be true of matching agents.
      *
      * <!-- here we see a FOAF group described.
      * each foaf group may be associated with an OWL definition
      * specifying the class of agents that constitute the group's membership -->
      * <foaf:Group>
      * <foaf:name>ILRT staff</foaf:name>
      * <foaf:membershipClass>
      * <owl:Class rdf:about="http://ilrt.example.com/groups#ILRTStaffPerson">
      * <rdfs:subClassOf rdf:resource="http://xmlns.com/foaf/0.1/Person"/>
      * <rdfs:subClassOf>
      * <owl:Restriction>
      * <owl:onProperty rdf:resource="http://xmlns.com/foaf/0.1/workplaceHomepage"/>
      * <owl:hasValue rdf:resource="http://www.ilrt.bris.ac.uk/"/>
      * </owl:Restriction>
      * </rdfs:subClassOf>
      * </owl:Class>
      * </foaf:membershipClass>
      * </foaf:Group>
      *
      * Note that while these example OWL rules for being in the eg:ILRTStaffPerson class are based on a Person having
      * a particular workplaceHomepage, this places no obligations on the authors of actual FOAF documents to include
      * this information. If the information is included, then generic OWL tools may infer that some person is
      * an eg:ILRTStaffPerson. To go the extra step and infer that some eg:ILRTStaffPerson is a member of the group
      * whose name is "ILRT staff", tools will need some knowledge of the way FOAF deals with groups. In other words,
      * generic OWL technology gets us most of the way, but the full Group machinery requires extra work for implimentors.
      *
      * The current design names the relationship as pointing from the group, to the member. This is convenient when
      * writing XML/RDF that encloses the members within markup that describes the group. Alternate representations of
      * the same content are allowed in RDF, so you can write claims about the Person and the Group without having to
      * nest either description inside the other. For (brief) example:
      *
      * <foaf:Group>
      * <foaf:member rdf:nodeID="martin"/>
      * <!-- more about the group here -->
      * </foaf:Group>
      * <foaf:Person rdf:nodeID="martin">
      * <!-- more about martin here -->
      * </foaf:Person>
      *
      * There is a FOAF issue tracker associated with this FOAF term. A design goal is to make the most of W3C's OWL
      * language for representing group-membership criteria, while also making it easy to leverage existing groups and
      * datasets available online (eg. buddylists, mailing list membership lists etc). Feedback on the current design is
      * solicited! Should we consider using SPARQL queries instead, for example?
      */
    val Group = resource("Group")

    /**
      * Class: foaf:Organization
      * Organization - An organization.
      * Status: 	stable
      * Subclass Of 	Agent
      * Disjoint With: 	Person Document
      *
      * The Organization class represents a kind of Agent corresponding to social instititutions such as companies, societies etc.
      *
      * This is a more 'solid' class than Group, which allows for more ad-hoc collections of individuals. These terms,
      * like the corresponding natural language concepts, have some overlap, but different emphasis.
      */
    val Organization = resource("Organization")

    /**
      * Class: foaf:Person
      * Person - A person.
      * Status: 	stable
      * Properties include: 	myersBriggs familyName publications lastName family_name firstName currentProject surname
      * knows workInfoHomepage pastProject geekcode schoolHomepage workplaceHomepage img plan
      * Used with: 	knows
      * Subclass Of 	Agent Spatial Thing Person
      * Disjoint With: 	Organization Project
      *
      * The Person class represents people. Something is a Person if it is a person. We don't nitpic about whether
      * they're alive, dead, real, or imaginary. The Person class is a sub-class of the Agent class, since all people are
      * considered 'agents' in FOAF.
      */
    val Person = resource("Person")

    /**
      * Class: foaf:Document
      * Document - A document.
      * Status: 	testing
      * Properties include: 	topic sha1 primaryTopic
      * Used with: 	weblog openid tipjar accountServiceHomepage isPrimaryTopicOf workplaceHomepage homepage interest
      * workInfoHomepage page publications schoolHomepage
      * Has Subclass 	PersonalProfileDocument Image
      * Disjoint With: 	Organization Project
      *
      * The Document class represents those things which are, broadly conceived, 'documents'.
      *
      * The Image class is a sub-class of Document, since all images are documents.
      *
      * We do not (currently) distinguish precisely between physical and electronic documents, or between copies of a work
      * and the abstraction those copies embody. The relationship between documents and their byte-stream representation
      * needs clarification (see sha1 for related issues).
      */
    val Document = resource("Document")

    /**
      * Class: foaf:Image
      * Image - An image.
      * Status: 	testing
      * Properties include: 	thumbnail depicts
      * Used with: 	thumbnail depiction img
      * Subclass Of 	Document
      *
      * The class Image is a sub-class of Document corresponding to those documents which are images.
      *
      * Digital images (such as JPEG, PNG, GIF bitmaps, SVG diagrams etc.) are examples of Image.
      */
    val Image = resource("Image")

    /**
      * Class: foaf:OnlineAccount
      * Online Account - An online account.
      * Status: 	testing
      * Properties include: 	accountServiceHomepage accountName
      * Used with: 	account holdsAccount
      * Subclass Of 	Thing
      * Has Subclass 	Online E-commerce Account Online Gaming Account Online Chat Account
      *
      * The OnlineAccount class represents the provision of some form of online service, by some party
      * (indicated indirectly via a accountServiceHomepage) to some Agent. The account property of the agent is used
      * to indicate accounts that are associated with the agent.
      *
      * See OnlineChatAccount for an example. Other sub-classes include OnlineEcommerceAccount and OnlineGamingAccount.
      *
      * One deployment style for this construct is to use URIs for well-known documents (or other entities) that strongly
      * embody the account-holding relationship; for example, user profile pages on social network sites. This has the
      * advantage of providing URIs that are likely to be easy to link with other information, but means that the instances
      * of this class should not be considered 'accounts' in the abstract or business sense of a 'contract'.
      */
    val OnlineAccount = resource("OnlineAccount")
    /**
      * PersonalProfileDocument - A personal profile RDF document.
      * Status: 	testing
      * Subclass Of 	Document
      *
      * The PersonalProfileDocument class represents those things that are a Document, and that use RDF to describe properties
      * of the person who is the maker of the document. There is just one Person described in the document, ie. the person
      * who made it and who will be its primaryTopic.
      *
      * The PersonalProfileDocument class, and FOAF's associated conventions for describing it, captures an important
      * deployment pattern for the FOAF vocabulary. FOAF is very often used in public RDF documents made available
      * through the Web. There is a colloquial notion that these "FOAF files" are often somebody's FOAF file.
      * Through PersonalProfileDocument we provide a machine-readable expression of this concept, providing a basis
      * for FOAF documents to make claims about their maker and topic.
      *
      * When describing a PersonalProfileDocument it is typical (and useful) to describe its associated Person using the maker
      * property. Anything that is a Person and that is the maker of some PersonalProfileDocument will be the primaryTopic
      * of that Document. Although this can be inferred, it is often helpful to include this information explicitly within
      * the PersonalProfileDocument.
      *
      * For example, here is a fragment of a personal profile document which describes its author explicitly:
      *
      * <foaf:Person rdf:nodeID="p1">
      * <foaf:name>Dan Brickley</foaf:name>
      * <foaf:homepage rdf:resource="http://danbri.org/"/>
      * <!-- etc... -->
      * </foaf:Person>
      *
      * <foaf:PersonalProfileDocument rdf:about="">
      * <foaf:maker rdf:nodeID="p1"/>
      * <foaf:primaryTopic rdf:nodeID="p1"/>
      * </foaf:PersonalProfileDocument>
      *
      * Note that a PersonalProfileDocument will have some representation as RDF. Typically this will be in W3C's RDF/XML
      * syntax, however we leave open the possibility for the use of other notations, or representational conventions
      * including automated transformations from HTML (GRDDL spec for one such technique).
      */
    val PersonalProfileDocument = resource("PersonalProfileDocument")

    /**
      * Class: foaf:Project
      * Project - A project (a collective endeavour of some kind).
      * Status: 	testing
      * Disjoint With: 	Person Document
      *
      * The Project class represents the class of things that are 'projects'. These may be formal or informal, collective or
      * individual. It is often useful to indicate the homepage of a Project.
      *
      * Further work is needed to specify the connections between this class and the FOAF properties currentProject and pastProject.
      */
    val Project = resource("Project")

    /**
      * Class: foaf:LabelProperty
      * Label Property - A foaf:LabelProperty is any RDF property with texual values that serve as labels.
      * Status: 	unstable
      *
      * A LabelProperty is any RDF property with texual values that serve as labels.
      *
      * Any property that is a LabelProperty is effectively a sub-property of rdfs:label. This utility class provides an
      * alternate means of expressing this idea, in a way that may help with OWL 2.0 DL compatibility.
      */
    val LabelProperty = resource("LabelProperty")

    /**
      * Class: foaf:OnlineChatAccount
      * Online Chat Account - An online chat account.
      * Status: 	unstable
      * Subclass Of 	Online Account
      *
      * A OnlineChatAccount is a OnlineAccount devoted to chat / instant messaging. The account may offer other services too;
      * FOAF's sub-classes of OnlineAccount are not mutually disjoint.
      *
      * This is a generalization of the FOAF Chat ID properties, jabberID, aimChatID, skypeID, msnChatID, icqChatID and yahooChatID.
      *
      * Unlike those simple properties, OnlineAccount and associated FOAF terms allows us to describe a great variety of online
      * accounts, without having to anticipate them in the FOAF vocabulary.
      *
      * For example, here is a description of an IRC chat account, specific to the Freenode IRC network:
      *
      * <foaf:Person>
      * <foaf:name>Dan Brickley</foaf:name>
      * <foaf:account>
      * <foaf:OnlineAccount>
      * <rdf:type rdf:resource="http://xmlns.com/foaf/0.1/OnlineChatAccount"/>
      * <foaf:accountServiceHomepage
      * rdf:resource="http://www.freenode.net/"/>
      * <foaf:accountName>danbri</foaf:accountName>
      * </foaf:OnlineAccount>
      * </foaf:account>
      * </foaf:Person>
      *
      * Note that it may be impolite to carelessly reveal someone else's chat identifier (which might also serve as an indicate
      * of email address) As with email, there are privacy and anti-SPAM considerations. FOAF does not currently provide a
      * way to represent an obfuscated chat ID (ie. there is no parallel to the mbox / mbox_sha1sum mapping).
      *
      * In addition to the generic OnlineAccount and OnlineChatAccount mechanisms, FOAF also provides several convenience chat
      * ID properties (jabberID, aimChatID, icqChatID, msnChatID,yahooChatID, skypeID). These serve as as a shorthand for
      * some common cases; their use may not always be appropriate.
      *
      * We should specify some mappings between the abbreviated and full representations of Jabber, AIM, MSN, ICQ, Yahoo! and
      * MSN chat accounts. This has been done for skypeID. This requires us to identify an appropriate accountServiceHomepage
      * for each. If we wanted to make the OnlineAccount mechanism even more generic, we could invent a relationship that
      * holds between a OnlineAccount instance and a convenience property. To continue the example above, we could describe
      * how Freenode could define a property 'fn:freenodeChatID' corresponding to Freenode online accounts.
      *
      *
      */
    val OnlineChatAccount = resource("OnlineChatAccount")

    /**
      * Class: foaf:OnlineEcommerceAccount
      * Online E-commerce Account - An online e-commerce account.
      * Status: 	unstable
      * Subclass Of 	Online Account
      *
      * A OnlineEcommerceAccount is a OnlineAccount devoted to buying and/or selling of goods, services etc.
      * Examples include Amazon, eBay, PayPal, thinkgeek, etc
      */
    val OnlineEcommerceAccount = resource("OnlineEcommerceAccount")

    /**
      * Class: foaf:OnlineGamingAccount
      * Online Gaming Account - An online gaming account.
      * Status: 	unstable
      * Subclass Of 	Online Account
      *
      * A OnlineGamingAccount is a OnlineAccount devoted to online gaming.
      *
      * Examples might include EverQuest, Xbox live, Neverwinter Nights, etc., as well as older text-based systems (MOOs, MUDs and suchlike).
      */
    val OnlineGamingAccount = resource("OnlineGamingAccount")

    /**
      * Property: foaf:homepage
      * homepage - A homepage for some thing.
      * Status: 	stable
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Document
      * Inverse Functional Property
      *
      * The homepage property relates something to a homepage about it.
      *
      * Many kinds of things have homepages. FOAF allows a thing to have multiple homepages, but constrains homepage
      * so that there can be only one thing that has any particular homepage.
      *
      * A 'homepage' in this sense is a public Web document, typically but not necessarily available in HTML format.
      * The page has as a topic the thing whose homepage it is. The homepage is usually controlled, edited or published
      * by the thing whose homepage it is; as such one might look to a homepage for information on its owner from its owner.
      * This works for people, companies, organisations etc.
      *
      * The homepage property is a sub-property of the more general page property for relating a thing to a page about
      * that thing. See also topic, the inverse of the page property.
      */
    val homepage = property("homepage")

    /**
      * Property: foaf:isPrimaryTopicOf
      * is primary topic of - A document that this thing is the primary topic of.
      * Status: 	stable
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Document
      * Inverse Functional Property
      *
      * The isPrimaryTopicOf property relates something to a document that is mainly about it.
      *
      * The isPrimaryTopicOf property is inverse functional: for any document that is the value of this property, there is at
      * most one thing in the world that is the primary topic of that document. This is useful, as it allows for data merging,
      * as described in the documentation for its inverse, primaryTopic.
      *
      * page is a super-property of isPrimaryTopicOf. The change of terminology between the two property names reflects the
      * utility of 'primaryTopic' and its inverse when identifying things. Anything that has an isPrimaryTopicOf relation
      * to some document X, also has a page relationship to it.
      *
      * Note that homepage, is a sub-property of both page and isPrimaryTopicOf. The awkwardly named isPrimaryTopicOf is less
      * specific, and can be used with any document that is primarily about the thing of interest (ie. not just on homepages).
      */
    val isPrimaryTopicOf = property("isPrimaryTopicOf")

    /**
      * Property: foaf:knows
      * knows - A person known by this person (indicating some level of reciprocated interaction between the parties).
      * Status: 	stable
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Person
      *
      * The knows property relates a Person to another Person that he or she knows.
      *
      * We take a broad view of 'knows', but do require some form of reciprocated interaction (ie. stalkers need not apply).
      * Since social attitudes and conventions on this topic vary greatly between communities, counties and cultures, it is
      * not appropriate for FOAF to be overly-specific here.
      *
      * If someone knows a person, it would be usual for the relation to be reciprocated. However this doesn't mean that there
      * is any obligation for either party to publish FOAF describing this relationship. A knows relationship does not imply
      * friendship, endorsement, or that a face-to-face meeting has taken place: phone, fax, email, and smoke signals are all
      * perfectly acceptable ways of communicating with people you know.
      *
      * You probably know hundreds of people, yet might only list a few in your public FOAF file. That's OK. Or you might list
      * them all. It is perfectly fine to have a FOAF file and not list anyone else in it at all. This illustrates the Semantic
      * Web principle of partial description: RDF documents rarely describe the entire picture. There is always more to be said,
      * more information living elsewhere in the Web (or in our heads...).
      *
      * Since knows is vague by design, it may be suprising that it has uses. Typically these involve combining other RDF
      * properties. For example, an application might look at properties of each weblog that was made by someone you "knows".
      * Or check the newsfeed of the online photo archive for each of these people, to show you recent photos taken by people
      * you know.
      *
      * To provide additional levels of representation beyond mere 'knows', FOAF applications can do several things.
      *
      * They can use more precise relationships than knows to relate people to people. The original FOAF design included two of
      * these ('knowsWell','friend') which we removed because they were somewhat awkward to actually use, bringing an inappopriate a
      * ir of precision to an intrinsically vague concept. Other extensions have been proposed, including Eric Vitiello's
      * Relationship module for FOAF.
      *
      * In addition to using more specialised inter-personal relationship types (eg rel:acquaintanceOf etc) it is often just as
      * good to use RDF descriptions of the states of affairs which imply particular kinds of relationship. So for example,
      * two people who have the same value for their workplaceHomepage property are typically colleagues. We don't (currently)
      * clutter FOAF up with these extra relationships, but the facts can be written in FOAF nevertheless. Similarly, if
      * there exists a Document that has two people listed as its makers, then they are probably collaborators of some kind.
      * Or if two people appear in 100s of digital photos together, there's a good chance they're friends and/or colleagues.
      *
      * So FOAF is quite pluralistic in its approach to representing relationships between people. FOAF is built on top of a
      * general purpose machine language for representing relationships (ie. RDF), so is quite capable of representing any
      * kinds of relationship we care to add. The problems are generally social rather than technical; deciding on appropriate
      * ways of describing these interconnections is a subtle art.
      *
      * Perhaps the most important use of knows is, alongside the rdfs:seeAlso property, to connect FOAF files together. Taken
      * alone, a FOAF file is somewhat dull. But linked in with 1000s of other FOAF files it becomes more interesting, with
      * each FOAF file saying a little more about people, places, documents, things... By mentioning other people (via knows
      * or other relationships), and by providing an rdfs:seeAlso link to their FOAF file, you can make it easy for FOAF indexing
      * tools ('scutters') to find your FOAF and the FOAF of the people you've mentioned. And the FOAF of the people they
      * mention, and so on. This makes it possible to build FOAF aggregators without the need for a centrally managed directory
      * of FOAF files...
      */
    val knows = property("knows")

    /**
      * Property: foaf:made
      * made - Something that was made by this agent.
      * Status: 	stable
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Thing
      *
      * The made property relates a Agent to something made by it. As such it is an inverse of the maker property, which relates
      * a thing to something that made it. See made for more details on the relationship between these FOAF terms and related
      * Dublin Core vocabulary.
      */
    val made = property("made")

    /**
      * Property: foaf:maker
      * maker - An agent that made this thing.
      * Status: 	stable
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Agent
      *
      * The maker property relates something to a Agent that made it. As such it is an inverse of the made property.
      *
      * The name (or other rdfs:label) of the maker of something can be described as the dc:creator of that thing.
      *
      * For example, if the thing named by the URI http://danbri.org/ has a maker that is a Person whose name is 'Dan Brickley',
      * we can conclude that http://danbri.org/ has a dc:creator of 'Dan Brickley'.
      *
      * FOAF descriptions are encouraged to use dc:creator only for simple textual names, and to use maker to indicate creators,
      * rather than risk confusing creators with their names. This follows most Dublin Core usage. See UsingDublinCoreCreator for details.
      */
    val maker = property("maker")

    /**
      * Property: foaf:mbox
      * personal mailbox - A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this
      * mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual
      * that ever has any particular value for foaf:mbox.
      * Status: 	stable
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Thing
      * Inverse Functional Property
      *
      * The mbox property is a relationship between the owner of a mailbox and a mailbox. These are typically identified using the
      * mailto: URI scheme (see RFC 2368).
      *
      * Note that there are many mailboxes (eg. shared ones) which are not the mbox of anyone. Furthermore, a person can have
      * multiple mbox properties.
      *
      * In FOAF, we often see mbox used as an indirect way of identifying its owner. This works even if the mailbox is itself
      * out of service (eg. 10 years old), since the property is defined in terms of its primary owner, and doesn't require
      * the mailbox to actually be being used for anything.
      *
      * Many people are wary of sharing information about their mailbox addresses in public. To address such concerns whilst
      * continuing the FOAF convention of indirectly identifying people by referring to widely known properties, FOAF also
      * provides the mbox_sha1sum mechanism, which is a relationship between a person and the value you get from passing a
      * mailbox URI to the SHA1 mathematical function.
      */
    val mbox = property("mbox")

    /**
      * Property: foaf:member
      * member - Indicates a member of a Group
      * Status: 	stable
      * Domain: 	having this property implies being a Group
      * Range: 	every value of this property is a Agent
      *
      * The member property relates a Group to a Agent that is a member of that group.
      *
      * See Group for details and examples.
      */
    val member = property("member")

    /**
      * Property: foaf:primaryTopic
      * primary topic - The primary topic of some page or document.
      * Status: 	stable
      * Domain: 	having this property implies being a Document
      * Range: 	every value of this property is a Thing
      * Functional Property
      *
      * The primaryTopic property relates a document to the main thing that the document is about.
      *
      * The primaryTopic property is functional: for any document it applies to, it can have at most one value. This is useful,
      * as it allows for data merging. In many cases it may be difficult for third parties to determine the primary topic of
      * a document, but in a useful number of cases (eg. descriptions of movies, restaurants, politicians, ...) it should be
      * reasonably obvious. Documents are very often the most authoritative source of information about their own primary topics,
      * although this cannot be guaranteed since documents cannot be assumed to be accurate, honest etc.
      *
      * It is an inverse of the isPrimaryTopicOf property, which relates a thing to a document primarily about that thing. The
      * choice between these two properties is purely pragmatic. When describing documents, we use primaryTopic former to
      * point to the things they're about. When describing things (people etc.), it is useful to be able to directly cite
      * documents which have those things as their main topic - so we use isPrimaryTopicOf. In this way, Web sites such as
      * Wikipedia or NNDB can provide indirect identification for the things they have descriptions of.
      */
    val primaryTopic = property("primaryTopic")

    /**
      * Property: foaf:account
      * account - Indicates an account held by this agent.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Online Account
      *
      * The account property relates a Agent to an OnlineAccount for which they are the sole account holder.
      * See OnlineAccount for usage details.
      */
    val account = property("account")

    /**
      * Property: foaf:accountName
      * account name - Indicates the name (identifier) associated with this online account.
      * Status: 	testing
      * Domain: 	having this property implies being a Online Account
      *
      * The accountName property of a OnlineAccount is a textual representation of the account name (unique ID) associated with that account.
      */
    val accountName = property("accountName")

    /**
      * Property: foaf:accountServiceHomepage
      * account service homepage - Indicates a homepage of the service provide for this online account.
      * Status: 	testing
      * Domain: 	having this property implies being a Online Account
      * Range: 	every value of this property is a Document
      *
      * The accountServiceHomepage property indicates a relationship between a OnlineAccount and the homepage of the supporting service provider.
      */
    val accountServiceHomepage = property("accountServiceHomepage")

    /**
      * Property: foaf:aimChatID
      * AIM chat ID - An AIM chat ID
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * The aimChatID property relates a Agent to a textual identifier ('screenname') assigned to them in the
      * AOL Instant Messanger (AIM) system. See AOL's AIM site for more details of AIM and AIM screennames. The iChat tools
      * from Apple also make use of AIM identifiers.
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val aimChatID = property("aimChatID")

    /**
      * Property: foaf:based_near
      * based near - A location that something is based near, for some broadly human notion of near.
      * Status: 	testing
      * Domain: 	having this property implies being a Spatial Thing
      * Range: 	every value of this property is a Spatial Thing
      *
      * The based_near relationship relates two "spatial things" (anything that can be somewhere), the latter typically described
      * using the geo:lat / geo:long geo-positioning vocabulary (See GeoInfo in the W3C semweb wiki for details). This allows
      * us to say describe the typical latitute and longitude of, say, a Person (people are spatial things - they can be places)
      * without implying that a precise location has been given.
      *
      * We do not say much about what 'near' means in this context; it is a 'rough and ready' concept. For a more precise treatment,
      * see GeoOnion vocab design discussions, which are aiming to produce a more sophisticated vocabulary for such purposes.
      *
      * FOAF files often make use of the contact:nearestAirport property. This illustrates the distinction between FOAF documents
      * (which may make claims using any RDF vocabulary) and the core FOAF vocabulary defined by this specification. For further
      * reading on the use of nearestAirport see UsingContactNearestAirport in the FOAF wiki.
      */
    val based_near = property("based_near")

    /**
      * Property: foaf:currentProject
      * current project - A current project this person works on.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Thing
      *
      * A currentProject relates a Person to a Document indicating some collaborative or individual undertaking. This
      * relationship indicates that the Person has some active role in the project, such as development, coordination, or
      * support.
      *
      * When a Person is no longer involved with a project, or perhaps is inactive for some time, the relationship becomes a pastProject.
      *
      * If the Person has stopped working on a project because it has been completed (successfully or otherwise), pastProject is
      * applicable. In general, currentProject is used to indicate someone's current efforts (and implied interests, concerns etc.),
      * while pastProject describes what they've previously been doing.
      *
      * Note that this property requires further work. There has been confusion about whether it points to a
      * thing (eg. something you've made; a homepage for a project, ie. a Document or to instances of the class
      * Project, which might themselves have a homepage. In practice, it seems to have been used in a similar way to interest,
      * referencing homepages of ongoing projects.
      */
    val currentProject = property("currentProject")

    /**
      * Property: foaf:depiction
      * depiction - A depiction of some thing.
      * Status: 	testing
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Image
      *
      * The depiction property is a relationship between a thing and an Image that depicts it. As such it is an inverse of the
      * depicts relationship.
      *
      * A common use of depiction (and depicts) is to indicate the contents of a digital image, for example the people or objects
      * represented in an online photo gallery.
      *
      * Extensions to this basic idea include 'Co-Depiction' (social networks as evidenced in photos), as well as richer photo
      * metadata through the mechanism of using SVG paths to indicate the regions of an image which depict some particular
      * thing. See 'Annotating Images With SVG' for tools and details.
      *
      * The basic notion of 'depiction' could also be extended to deal with multimedia content (video clips, audio), or refined
      * to deal with corner cases, such as pictures of pictures etc.
      *
      * The depiction property is a super-property of the more specific property img, which is used more sparingly. You stand in
      * a depiction relation to any Image that depicts you, whereas img is typically used to indicate a few images that are
      * particularly representative.
      */
    val depiction = property("depiction")

    /**
      * Property: foaf:depicts
      * depicts - A thing depicted in this representation.
      * Status: 	testing
      * Domain: 	having this property implies being a Image
      * Range: 	every value of this property is a Thing
      *
      * The depicts property is a relationship between a Image and something that the image depicts. As such it is an inverse of
      * the depiction relationship. See depiction for further notes.
      */
    val depicts = property("depicts")

    /**
      * Property: foaf:familyName
      * familyName - The family name of some person.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      *
      * The familyName property is provided (alongside givenName) for use when describing parts of people's names. Although
      * these concepts do not capture the full range of personal naming styles found world-wide, they are commonly used
      * and have some value.
      *
      * There is also a simple name property.
      *
      * Support is also provided for the more archaic and culturally varying terminology of firstName and lastName.
      *
      * See the issue tracker for design discussions, status and ongoing work on rationalising the FOAF naming machinery.
      */
    val familyName = property("familyName")

    /**
      * Property: foaf:firstName
      * firstName - The first name of a person.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      *
      * The firstName property is provided (alongside lastName) as a mechanism to support legacy data that cannot be easily
      * interpreted in terms of the (otherwise preferred) familyName and givenName properties. The concepts of
      * 'first' and 'last' names do not work well across cultural and linguistic boundaries; however they are widely used
      * in addressbooks and databases.
      *
      * See the issue tracker for design discussions, status and ongoing work on rationalising the FOAF naming machinery.
      *
      * There is also a simple name property.
      */
    val firstName = property("firstName")

    /**
      * Property: foaf:focus
      * focus - The underlying or 'focal' entity associated with some SKOS-described concept.
      * Status: 	testing
      * Domain: 	having this property implies being a Concept
      * Range: 	every value of this property is a Thing
      *
      * The focus property relates a conceptualisation of something to the thing itself. Specifically, it is designed for use
      * with W3C's SKOS vocabulary, to help indicate specific individual things (typically people, places, artifacts) that
      * are mentioned in different SKOS schemes (eg. thesauri).
      *
      * W3C SKOS is based around collections of linked 'concepts', which indicate topics, subject areas and categories. In SKOS,
      * properties of a skos:Concept are properties of the conceptualization (see 2005 discussion for details); for example
      * administrative and record-keeping metadata. Two schemes might have an entry for the same individual; the foaf:focus
      * property can be used to indicate the thing in they world that they both focus on. Many SKOS concepts don't work this
      * way; broad topical areas and subject categories don't typically correspond to some particular entity. However, in
      * cases when they do, it is useful to link both subject-oriented and thing-oriented information via foaf:focus.
      *
      * FOAF's focus property works alongside its other topic-oriented constructs: topic, primaryTopic are used when talking
      * about the topical emphasis of a document. The notion of primaryTopic is particularly important in FOAF as it
      * provides an indirect mechanism for identifying things indirectly. A similar approach is explored by the TDB URI
      * scheme. FOAF includes topic-oriented functionality to address its original goals of linking people to information,
      * as well as to other people, through the use of linked information.
      */
    val focus = property("focus")

    /**
      * Property: foaf:gender
      * gender - The gender of this Agent (typically but not necessarily 'male' or 'female').
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Functional Property
      *
      * The gender property relates a Agent (typically a Person) to a string representing its gender. In most cases the value
      * will be the string 'female' or 'male' (in lowercase without surrounding quotes or spaces). Like all FOAF properties,
      * there is in general no requirement to use gender in any particular document or description. Values other than
      * 'male' and 'female' may be used, but are not enumerated here. The gender mechanism is not intended to capture the
      * full variety of biological, social and sexual concepts associated with the word 'gender'.
      *
      * Anything that has a gender property will be some kind of Agent. However there are kinds of Agent to which the concept
      * of gender isn't applicable (eg. a Group). FOAF does not currently include a class corresponding directly to
      * "the type of thing that has a gender". At any point in time, a Agent has at most one value for gender. FOAF does
      * not treat gender as a static property; the same individual may have different values for this property at different
      * times.
      *
      * Note that FOAF's notion of gender isn't defined biologically or anatomically - this would be tricky since we have a broad
      * notion that applies to all Agents (including robots - eg. Bender from Futurama is 'male'). As stressed above, FOAF's
      * notion of gender doesn't attempt to encompass the full range of concepts associated with human gender, biology and
      * sexuality. As such it is a (perhaps awkward) compromise between the clinical and the social/psychological. In general,
      * a person will be the best authority on their gender. Feedback on this design is particularly welcome (via the FOAF
      * mailing list, foaf-dev). We have tried to be respectful of diversity without attempting to catalogue or enumerate that
      * diversity.
      *
      * This may also be a good point for a periodic reminder: as with all FOAF properties, documents that use 'gender' will
      * on occassion be innacurate, misleading or outright false. FOAF, like all open means of communication, supports lying.
      * Application authors using FOAF data should always be cautious in their presentation of unverified information, but
      * be particularly sensitive to issues and risks surrounding sex and gender (including privacy and personal safety concerns).
      * Designers of FOAF-based user interfaces should be careful to allow users to omit gender when describing themselves
      * and others, and to allow at least for values other than 'male' and 'female' as options. Users of information conveyed
      * via FOAF (as via information conveyed through mobile phone text messages, email, Internet chat, HTML pages etc.)
      * should be skeptical of unverified information.
      */
    val gender = property("gender")

    /**
      * Property: foaf:givenName
      * Given name - The given name of some person.
      * Status: 	testing
      *
      * The givenName property is provided (alongside familyName) for use when describing parts of people's names. Although
      * these concepts do not capture the full range of personal naming styles found world-wide, they are commonly used
      * and have some value.
      *
      * There is also a simple name property.
      *
      * Support is also provided for the more archaic and culturally varying terminology of firstName and lastName.
      *
      * See the issue tracker for design discussions, status and ongoing work on rationalising the FOAF naming machinery.
      */
    val givenName = property("givenName")

    /**
      * Property: foaf:icqChatID
      * ICQ chat ID - An ICQ chat ID
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * The icqChatID property relates a Agent to a textual identifier assigned to them in the ICQ Chat system. See the icq
      * chat site for more details of the 'icq' service. Their "What is ICQ?" document provides a basic overview, while
      * their "About Us page notes that ICQ has been acquired by AOL. Despite the relationship with AOL, ICQ is at the time
      * of writing maintained as a separate identity from the AIM brand (see aimChatID).
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val icqChatID = property("icqChatID")

    /**
      * Property: foaf:img
      * image - An image that can be used to represent some thing (ie. those depictions which are particularly representative of
      * something, eg. one's photo on a homepage).
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Image
      *
      * The img property relates a Person to a Image that represents them. Unlike its super-property depiction, we only use img
      * when an image is particularly representative of some person. The analogy is with the image(s) that might appear on
      * someone's homepage, rather than happen to appear somewhere in their photo album.
      *
      * Unlike the more general depiction property (and its inverse, depicts), the img property is only used with representations
      * of people (ie. instances of Person). So you can't use it to find pictures of cats, dogs etc. The basic idea is to
      * have a term whose use is more restricted than depiction so we can have a useful way of picking out a reasonable
      * image to represent someone. FOAF defines img as a sub-property of depiction, which means that the latter relationship
      * is implied whenever two things are related by the former.
      *
      * Note that img does not have any restrictions on the dimensions, colour depth, format etc of the Image it references.
      *
      * Terminology: note that img is a property (ie. relationship), and that code:Image is a similarly named class
      * (ie. category, a type of thing). It might have been more helpful to call img 'mugshot' or similar; instead it
      * is named by analogy to the HTML IMG element.
      */
    val img = property("img")

    /**
      * Property: foaf:interest
      * interest - A page about a topic of interest to this person.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Document
      *
      * The interest property represents an interest of a Agent, through indicating a Document whose topic(s) broadly
      * characterises that interest.
      *
      * For example, we might claim that a person or group has an interest in RDF by saying they stand in a interest relationship
      * to the RDF home page. Loosly, such RDF would be saying "this agent is interested in the topic of this page".
      *
      * Uses of interest include a variety of filtering and resource discovery applications. It could be used, for example,
      * to help find answers to questions such as "Find me members of this organisation with an interest in XML who have
      * also contributed to CPAN)".
      *
      * This approach to characterising interests is intended to compliment other mechanisms (such as the use of controlled
      * vocabulary). It allows us to use a widely known set of unique identifiers (Web page URIs) with minimal
      * pre-coordination. Since URIs have a controlled syntax, this makes data merging much easier than the use of
      * free-text characterisations of interest.
      *
      * Note that interest does not imply expertise, and that this FOAF term provides no support for characterising levels of
      * interest: passing fads and lifelong quests are both examples of someone's interest. Describing interests in full is
      * a complex undertaking; interest provides one basic component of FOAF's approach to these problems.
      */
    val interest = property("interest")

    /**
      * Property: foaf:jabberID
      * jabber ID - A jabber ID for something.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * The jabberID property relates a Agent to a textual identifier assigned to them in the Jabber messaging system. See
      * the Jabber site for more information about the Jabber protocols and tools.
      *
      * Jabber, unlike several other online messaging systems, is based on an open, publically documented protocol specification,
      * and has a variety of open source implementations. Jabber IDs can be assigned to a variety of kinds of thing,
      * including software 'bots', chat rooms etc. For the purposes of FOAF, these are all considered to be kinds of Agent
      * (ie. things that do stuff). The uses of Jabber go beyond simple IM chat applications. The jabberID property is
      * provided as a basic hook to help support RDF description of Jabber users and services.
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val jabberID = property("jabberID")

    /**
      * Property: foaf:lastName
      * lastName - The last name of a person.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      *
      * The lastName property is provided (alongside firstName) as a mechanism to support legacy data that cannot be easily
      * interpreted in terms of the (otherwise preferred) familyName and givenName properties. The concepts of 'first' and
      * 'last' names do not work well across cultural and linguistic boundaries; however they are widely used in addressbooks
      * and databases.
      *
      * See the issue tracker for design discussions, status and ongoing work on rationalising the FOAF naming machinery.
      *
      * There is also a simple name property.
      */
    val lastName = property("lastName")

    /**
      * Property: foaf:logo
      * logo - A logo representing some thing.
      * Status: 	testing
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Thing
      * Inverse Functional Property
      *
      * The logo property is used to indicate a graphical logo of some kind. It is probably underspecified...
      *
      * [#] [wiki] [back to top]
      *
      */
    val logo = property("logo")

    /**
      * Property: foaf:mbox_sha1sum
      * sha1sum of a personal mailbox URI name - The sha1sum of the URI of an Internet mailbox associated with exactly one
      * owner, the first owner of the mailbox.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * A mbox_sha1sum of a Person is a textual representation of the result of applying the SHA1 mathematical functional to a
      * 'mailto:' identifier (URI) for an Internet mailbox that they stand in a mbox relationship to.
      *
      * In other words, if you have a mailbox (mbox) but don't want to reveal its address, you can take that address and
      * generate a mbox_sha1sum representation of it. Just as a mbox can be used as an indirect identifier for its owner,
      * we can do the same with mbox_sha1sum since there is only one Person with any particular value for that property.
      *
      * Many FOAF tools use mbox_sha1sum in preference to exposing mailbox information. This is usually for privacy and
      * SPAM-avoidance reasons. Other relevant techniques include the use of PGP encryption (see Edd Dumbill's documentation)
      * and the use of FOAF-based whitelists for mail filtering.
      *
      * Code examples for SHA1 in C#, Java, PHP, Perl and Python can be found in Sam Ruby's weblog entry. Remember to include
      * the 'mailto:' prefix, but no trailing whitespace, when computing a mbox_sha1sum property.
      */
    val mbox_sha1sum = property("mbox_sha1sum")

    /**
      * Property: foaf:msnChatID
      * MSN chat ID - An MSN chat ID
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * The msnChatID property relates a Agent to a textual identifier assigned to them in the Microsoft online chat system
      * originally known as 'MSN', and now Windows Live Messenger. See the Microsoft mesenger and Windows Live ID sites
      * for more details.
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val msnChatID = property("msnChatID")

    /**
      * Property: foaf:myersBriggs
      * myersBriggs - A Myers Briggs (MBTI) personality classification.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      *
      * The myersBriggs property represents the Myers Briggs (MBTI) approach to personality taxonomy. It is included in FOAF
      * as an example of a property that takes certain constrained values, and to give some additional detail to the FOAF
      * files of those who choose to include it. The myersBriggs property applies only to the Person class; wherever you
      * see it, you can infer it is being applied to a person.
      *
      * The myersBriggs property is interesting in that it illustrates how FOAF can serve as a carrier for various kinds of
      * information, without necessarily being commited to any associated worldview. Not everyone will find myersBriggs
      * (or star signs, or blood types, or the four humours) a useful perspective on human behaviour and personality. The
      * inclusion of a Myers Briggs property doesn't indicate that FOAF endorses the underlying theory, any more than the
      * existence of weblog is an endorsement of soapboxes.
      *
      * The values for myersBriggs are the following 16 4-letter textual codes: ESTJ, INFP, ESFP, INTJ, ESFJ, INTP, ENFP, ISTJ,
      * ESTP, INFJ, ENFJ, ISTP, ENTJ, ISFP, ENTP, ISFJ. If multiple of these properties are applicable, they are represented
      * by applying multiple properties to a person.
      *
      * For further reading on MBTI, see various online sources (eg. this article). There are various online sites which offer
      * quiz-based tools for determining a person's MBTI classification. The owners of the MBTI trademark have probably not
      * approved of these.
      *
      * This FOAF property suggests some interesting uses, some of which could perhaps be used to test the claims made by
      * proponents of the MBTI (eg. an analysis of weblog postings filtered by MBTI type). However it should be noted that
      * MBTI FOAF descriptions are self-selecting; MBTI categories may not be uniformly appealing to the people they
      * describe.
      * Further, there is probably a degree of cultural specificity implicit in the assumptions made by many
      * questionaire-based MBTI tools; the MBTI system may not make sense in cultural settings beyond those it was created for.
      *
      * See also Cory Caplinger's summary table or the RDFWeb article, FOAF Myers Briggs addition for further background
      * and examples.
      *
      * Note: Myers Briggs Type Indicator and MBTI are registered trademarks of Consulting Psychologists Press Inc.
      * Oxford Psycholgists Press Ltd has exclusive rights to the trademark in the UK.
      */
    val myersBriggs = property("myersBriggs")

    /**
      * Property: foaf:name
      * name - A name for some thing.
      * Status: 	testing
      * Domain: 	having this property implies being a Thing
      *
      * The name of something is a simple textual string.
      *
      * XML language tagging may be used to indicate the language of the name. For example:
      * <foaf:name xml:lang="en">Dan Brickley</foaf:name>
      *
      * FOAF provides some other naming constructs. While foaf:name does not explicitly represent name
      * substructure (family vs given etc.) it does provide a basic level of interoperability. See the issue tracker for
      * status of work on this issue.
      *
      * The name property, like all RDF properties with a range of rdfs:Literal, may be used with XMLLiteral datatyped values
      * (multiple names are acceptable whether they are in the same langauge or not). XMLLiteral usage is not yet widely adopted.
      * Feedback on this aspect of the FOAF design is particularly welcomed.
      */
    val name = property("name")

    /**
      * Property: foaf:nick
      * nickname - A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames).
      * Status: 	testing
      *
      * The nick property relates a Person to a short (often abbreviated) nickname, such as those use in IRC chat,
      * online accounts, and computer logins.
      *
      * This property is necessarily vague, because it does not indicate any particular naming control authority, and so cannot
      * distinguish a person's login from their (possibly various) IRC nicknames or other similar identifiers. However
      * it has some utility, since many people use the same string (or slight variants) across a variety of such environments.
      *
      * For specific controlled sets of names (relating primarily to Instant Messanger accounts), FOAF provides some
      * convenience properties: jabberID, aimChatID, msnChatID and icqChatID. Beyond this, the problem of representing
      * such accounts is not peculiar to Instant Messanging, and it is not scaleable to attempt to enumerate each naming
      * database as a distinct FOAF property. The OnlineAccount term (and supporting vocabulary) are provided as a more
      * verbose and more expressive generalisation of these properties.
      */
    val nick = property("nick")
    /**
      * Property: foaf:openid
      * openid - An OpenID for an Agent.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Document
      * Inverse Functional Property
      *
      * A openid is a property of a Agent that associates it with a document that can be used as an indirect identifier
      * in the manner of the OpenID "Identity URL". As the OpenID 1.1 specification notes, OpenID itself"does not
      * provide any mechanism to exchange profile information, though Consumers of an Identity can learn more about
      * an End User from any public, semantically interesting documents linked thereunder (FOAF, RSS, Atom, vCARD, etc.)".
      * In this way, FOAF and OpenID complement each other; neither provides a stand-alone approach to online "trust",
      * but combined they can address interesting parts of this larger problem space.
      *
      * The openid property is "inverse functional", meaning that anything that is the foaf:openid of something, is the openid
      * of no more than one thing. FOAF is agnostic as to whether there are (according to the relevant OpenID specifications)
      * OpenID URIs that are equally associated with multiple Agents. FOAF offers sub-classes of Agent, ie. Organization and
      * Group, that allow for such scenarios to be consistent with the notion that any foaf:openid is the foaf:openid of
      * just one Agent.
      *
      * FOAF does not mandate any particular URI scheme for use as openid values. The OpenID 1.1 specification includes a
      * delegation model that is often used to allow a weblog or homepage document to also serve in OpenID authentication
      * via "link rel" HTML markup. This deployment model provides a convenient connection to FOAF, since a similar technique
      * is used for FOAF autodiscovery in HTML. A single document can, for example, serve both as a homepage and an OpenID
      * identity URL.
      */
    val openid = property("openid")

    /**
      * Property: foaf:page
      * page - A page or document about this thing.
      * Status: 	testing
      * Domain: 	having this property implies being a Thing
      * Range: 	every value of this property is a Document
      *
      * The page property relates a thing to a document about that thing.
      *
      * As such it is an inverse of the topic property, which relates a document to a thing that the document is about.
      */
    val page = property("page")

    /**
      * Property: foaf:pastProject
      * past project - A project this person has previously worked on.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Thing
      *
      * After a Person is no longer involved with a currentProject, or has been inactive for some time, a pastProject
      * relationship can be used. This indicates that the Person was involved with the described project at one point.
      *
      * If the Person has stopped working on a project because it has been completed (successfully or otherwise), pastProject
      * is applicable. In general, currentProject is used to indicate someone's current efforts (and implied interests,
      * concerns etc.), while pastProject describes what they've previously been doing.
      */
    val pastProject = property("pastProject")

    /**
      * Property: foaf:phone
      * phone - A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel).
      * Status: 	testing
      *
      * The phone of something is a phone, typically identified using the tel: URI scheme.
      */
    val phone = property("phone")

    /**
      * Property: foaf:plan
      * plan - A .plan comment, in the tradition of finger and '.plan' files.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      *
      * The plan property provides a space for a Person to hold some arbitrary content that would appear in a traditional
      * '.plan' file. The plan file was stored in a user's home directory on a UNIX machine, and displayed to people when
      * the user was queried with the finger utility.
      *
      * A plan file could contain anything. Typical uses included brief comments, thoughts, or remarks on what a person had been
      * doing lately. Plan files were also prone to being witty or simply osbscure. Others may be more creative, writing any
      * number of seemingly random compositions in their plan file for people to stumble upon.
      *
      * See History of the Finger Protocol by Rajiv Shah for more on this piece of Internet history. The geekcode property may a
      * lso be of interest.
      */
    val plan = property("plan")

    /**
      * Property: foaf:publications
      * publications - A link to the publications of this person.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Document
      *
      * The publications property indicates a Document listing (primarily in human-readable form) some publications associated
      * with the Person. Such documents are typically published alongside one's homepage.
      *
      *
      *
      */
    val publications = property("publications")

    /**
      * Property: foaf:schoolHomepage
      * schoolHomepage - A homepage of a school attended by the person.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Document
      *
      * The schoolHomepage property relates a Person to a Document that is the homepage of a School that the person attended.
      *
      * FOAF does not (currently) define a class for 'School' (if it did, it would probably be as a sub-class of Organization).
      * The original application area for schoolHomepage was for 'schools' in the British-English sense; however
      * American-English usage has dominated, and it is now perfectly reasonable to describe Universities, Colleges and
      * post-graduate study using schoolHomepage.
      *
      * This very basic facility provides a basis for a low-cost, decentralised approach to classmate-reunion and suchlike.
      * Instead of requiring a central database, we can use FOAF to express claims such as 'I studied here' simply by
      * mentioning a school's homepage within FOAF files. Given the homepage of a school, it is easy for FOAF aggregators
      * to lookup this property in search of people who attended that school.
      */
    val schoolHomepage = property("schoolHomepage")

    /**
      * Property: foaf:skypeID
      * Skype ID - A Skype ID
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      *
      * The skype property relates a Agent to an account name of a Skype account of theirs.
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val skypeID = property("skypeID")

    /**
      * Property: foaf:thumbnail
      * thumbnail - A derived thumbnail image.
      * Status: 	testing
      * Domain: 	having this property implies being a Image
      * Range: 	every value of this property is a Image
      *
      * The thumbnail property is a relationship between a full-size Image and a smaller, representative Image that has been
      * derrived from it.
      *
      * It is typical in FOAF to express img and depiction relationships in terms of the larger, 'main' (in some sense) image,
      * rather than its thumbnail(s). A thumbnail might be clipped or otherwise reduced such that it does not depict everything
      * that the full image depicts. Therefore FOAF does not specify that a thumbnail depicts everything that the image it is
      * derrived from depicts. However, FOAF does expect that anything depicted in the thumbnail will also be depicted in
      * the source image.
      *
      * A thumbnail is typically small enough that it can be loaded and viewed quickly before a viewer decides to download the
      * larger version. They are often used in online photo gallery applications.
      */
    val thumbnail = property("thumbnail")

    /**
      * Property: foaf:tipjar
      * tipjar - A tipjar document for this agent, describing means for payment and reward.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Document
      *
      * The tipjar property relates an Agent to a Document that describes some mechanisms for paying or otherwise rewarding
      * that agent.
      *
      * The tipjar property was created following discussions about simple, lightweight mechanisms that could be used to
      * encourage rewards and payment for content exchanged online. An agent's tipjar page(s) could describe informal
      * ("Send me a postcard!", "here's my book, music and movie wishlist") or formal (machine-readable micropayment i
      * nformation) information about how that agent can be paid or rewarded. The reward is not associated with any
      * particular action or content from the agent concerned. A link to a service such as PayPal is the sort of thing
      * we might expect to find in a tipjar document.
      *
      * Note that the value of a tipjar property is just a document (which can include anchors into HTML pages). We expect,
      * but do not currently specify, that this will evolve into a hook for finding more machine-readable information to
      * support payments, rewards. The OnlineAccount machinery is also relevant, although the information requirements for
      * automating payments are not currently clear.
      */
    val tipjar = property("tipjar")

    /**
      * Property: foaf:title
      * title - Title (Mr, Mrs, Ms, Dr. etc)
      * Status: 	testing
      *
      * This property is a candidate for deprecation in favour of 'honorificPrefix' following Portable Contacts usage.
      * See the FOAF Issue Tracker.
      *
      * The approriate values for title are not formally constrained, and will vary across community and context.
      * Values such as 'Mr', 'Mrs', 'Ms', 'Dr' etc. are expected.
      */
    val title = property("title")

    /**
      * Property: foaf:topic
      * topic - A topic of some page or document.
      * Status: 	testing
      * Domain: 	having this property implies being a Document
      * Range: 	every value of this property is a Thing
      *
      * The topic property relates a document to a thing that the document is about.
      *
      * As such it is an inverse of the page property, which relates a thing to a document about that thing.
      */
    val topic = property("topic")

    /**
      * Property: foaf:topic_interest
      * topic_interest - A thing of interest to this person.
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Thing
      *
      * The topic_interest property links a Agent to a thing that they're interested in. Unlike topic it is not indirected
      * through a document, but links the thing directly.
      */
    val topic_interest = property("topic_interest")

    /**
      * Property: foaf:weblog
      * weblog - A weblog of some thing (whether person, group, company etc.).
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Range: 	every value of this property is a Document
      * Inverse Functional Property
      *
      * The weblog property relates a Agent to a weblog of that agent.
      */
    val weblog = property("weblog")

    /**
      * Property: foaf:workInfoHomepage
      * work info homepage - A work info homepage of some person; a page about their work for some organization.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Document
      *
      * The workInfoHomepage of a Person is a Document that describes their work. It is generally (but not necessarily) a
      * different document from their homepage, and from any workplaceHomepage(s) they may have.
      *
      * The purpose of this property is to distinguish those pages you often see, which describe someone's professional role
      * within an organisation or project. These aren't really homepages, although they share some characterstics.
      */
    val workInfoHomepage = property("workInfoHomepage")

    /**
      * Property: foaf:workplaceHomepage
      * workplace homepage - A workplace homepage of some person; the homepage of an organization they work for.
      * Status: 	testing
      * Domain: 	having this property implies being a Person
      * Range: 	every value of this property is a Document
      *
      * The workplaceHomepage of a Person is a Document that is the homepage of a Organization that they work for.
      *
      * By directly relating people to the homepages of their workplace, we have a simple convention that takes advantage of a
      * set of widely known identifiers, while taking care not to confuse the things those identifiers
      * identify (ie. organizational homepages) with the actual organizations those homepages describe.
      *
      * For example, Dan Brickley works at W3C. Dan is a Person with a homepage of http://danbri.org/; W3C is a Organization
      * with a homepage of http://www.w3.org/. This allows us to say that Dan has a workplaceHomepage of http://www.w3.org/.
      *
      * <foaf:Person>
      * <foaf:name>Dan Brickley</foaf:name>
      * <foaf:workplaceHomepage rdf:resource="http://www.w3.org/"/>
      * </foaf:Person>
      *
      * Note that several other FOAF properties work this way; schoolHomepage is the most similar. In general, FOAF often
      * indirectly identifies things via Web page identifiers where possible, since these identifiers are widely used and
      * known. FOAF does not currently have a term for the name of the relation (eg. "workplace") that holds between a Person
      * and an Organization that they work for.
      */
    val workplaceHomepage = property("workplaceHomepage")

    /**
      * Property: foaf:yahooChatID
      * Yahoo chat ID - A Yahoo chat ID
      * Status: 	testing
      * Domain: 	having this property implies being a Agent
      * Inverse Functional Property
      *
      * The yahooChatID property relates a Agent to a textual identifier assigned to them in the Yahoo online Chat system.
      * See Yahoo's the Yahoo! Chat site for more details of their service. Yahoo chat IDs are also used across several
      * other Yahoo services, including email and Yahoo! Groups.
      *
      * See OnlineChatAccount (and OnlineAccount) for a more general (and verbose) mechanism for describing IM and chat accounts.
      */
    val yahooChatID = property("yahooChatID")

    /**
      * Property: foaf:age
      * age - The age in years of some agent.
      * Status: 	unstable
      * Domain: 	having this property implies being a Agent
      * Functional Property
      *
      * The age property is a relationship between a Agent and an integer string representing their age in years. See also birthday.
      */
    val age = property("age")

    /**
      * Property: foaf:birthday
      * birthday - The birthday of this Agent, represented in mm-dd string form, eg. '12-31'.
      * Status: 	unstable
      * Domain: 	having this property implies being a Agent
      * Functional Property
      *
      * The birthday property is a relationship between a Agent and a string representing the month and day in which they
      * were born (Gregorian calendar). See BirthdayIssue for details of related properties that can be used to describe
      * such things in more flexible ways.
      *
      * See also age.
      */
    val birthday = property("birthday")

    /**
      * Property: foaf:membershipClass
      * membershipClass - Indicates the class of individuals that are a member of a Group
      * Status: 	unstable
      *
      * The membershipClass property relates a Group to an RDF class representing a sub-class of Agent whose instances are
      * all the agents that are a member of the Group.
      *
      * See Group for details and examples.
      */
    val membershipClass = property("membershipClass")

    /**
      * Property: foaf:sha1
      * sha1sum (hex) - A sha1sum hash, in hex.
      * Status: 	unstable
      * Domain: 	having this property implies being a Document
      *
      * The sha1 property relates a Document to the textual form of a SHA1 hash of (some representation of) its contents.
      *
      * The design for this property is neither complete nor coherent. The Document class is currently used in a way that
      * allows multiple instances at different URIs to have the 'same' contents (and hence hash). If sha1 is an
      * owl:InverseFunctionalProperty, we could deduce that several such documents were the self-same thing. A more careful
      * design is needed, which distinguishes documents in a broad sense from byte sequences.
      */
    val sha1 = property("sha1")

    /**
      * Property: foaf:status
      * status - A string expressing what the user is happy for the general public (normally) to know about their current activity.
      * Status: 	unstable
      * Domain: 	having this property implies being a Agent
      *
      * status is a short textual string expressing what the user is happy for the general public (normally) to know
      * about their current activity. mood, location, etc.
      */
    val status = property("status")

  }

}
