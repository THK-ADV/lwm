package utils

import models.{ LabWorkGroup, LabworkGroups }
import utils.Global._
import utils.semantic.Vocabulary.lwm
import utils.semantic._

import scala.concurrent.Future

object ListGrouping {

  import scala.concurrent.ExecutionContext.Implicits.global

  case class Group(labwork: Resource) {
    var members = Vector.empty[Resource]

    val labworkIndividual = Individual(labwork)
    val id = labworkIndividual.props.getOrElse(lwm.hasGroup, List.empty[RDFNode]).size + 'A'
    val labworkGroup = LabWorkGroup(id.toChar.toString, labwork)
    val groupIndividual = LabworkGroups.create(labworkGroup)

    def addMember(member: Resource, application: Resource, partners: List[Resource]) = {
      def partnerGroupQuery(partner: Resource) = {
        s"""
           select ($partner as ?s) (${lwm.memberOf} as ?p) (?group as ?o) where {
            $application ${lwm.hasLabWork} ?labwork .
            ?labwork ${lwm.hasGroup} ?group .
            $partner ${lwm.memberOf} ?group .
            }
         """.stripMargin
      }

      def partnerApplicationQuery(partner: Resource) = {
        val query = s"""
           select ($partner as ?s) (${lwm.hasPendingApplication} as ?p) (?application as ?o) where {
            $application ${lwm.hasLabWork} ?labwork .
            $partner ${lwm.hasPendingApplication} ?application .
            ?application ${lwm.hasLabWork} ?labwork .
            }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      def partnersPartners(partner: Resource, application: Resource) = {
        val query = s"""
           select ($partner as ?s) (${lwm.hasPartner} as ?p) (?partner as ?o) where {
            $application ${lwm.hasPartner} ?partner
           }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).map(_.o.asResource().get)
      }

      val groups = partners.map { partner ⇒
        val query = partnerGroupQuery(partner)
        val result = sparqlExecutionContext.executeQueryBlocking(query)
        partner -> SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      groups.filter(_._2 == None).map { freePartner ⇒
        partnerApplicationQuery(freePartner._1).map { partnerApplication ⇒
          val pPartners = partnersPartners(freePartner._1, partnerApplication)
          if (pPartners.contains(member)) {
            val partner = Individual(freePartner._1)
            groupIndividual.map { i ⇒
              i.add(lwm.hasMember, freePartner._1)
              members = freePartner._1 +: members
              partner.add(lwm.memberOf, i.uri)
            }
            sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(partnerApplication))
          }
        }
      }

      groupIndividual.map { i ⇒
        i.add(lwm.hasMember, member)
        members = member +: members
        Individual(member).add(lwm.memberOf, i.uri)
      }

      sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(application))
    }

  }

  case class ExistingGroup(groupResource: Resource) {

    var i = Individual(groupResource)

    def members = i.props.get(lwm.hasMember).fold(List.empty[RDFNode])(list ⇒ list)

    def addMember(member: Resource, application: Resource, partners: List[Resource]) = {
      def partnerGroupQuery(partner: Resource) = {
        s"""
           select ($partner as ?s) (${lwm.memberOf} as ?p) (?group as ?o) where {
            $application ${lwm.hasLabWork} ?labwork .
            ?labwork ${lwm.hasGroup} ?group .
            $partner ${lwm.memberOf} ?group .
            }
         """.stripMargin
      }

      def partnerApplicationQuery(partner: Resource) = {
        val query = s"""
           select ($partner as ?s) (${lwm.hasPendingApplication} as ?p) (?application as ?o) where {
            $application ${lwm.hasLabWork} ?labwork .
            $partner ${lwm.hasPendingApplication} ?application .
            ?application ${lwm.hasLabWork} ?labwork .
            }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      def partnersPartners(partner: Resource, application: Resource) = {
        val query = s"""
           select ($partner as ?s) (${lwm.hasPartner} as ?p) (?partner as ?o) where {
            $application ${lwm.hasPartner} ?partner
           }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).map(_.o.asResource().get)
      }

      val groups = partners.map { partner ⇒
        val query = partnerGroupQuery(partner)
        val result = sparqlExecutionContext.executeQueryBlocking(query)
        partner -> SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      groups.filter(_._2 == None).map { freePartner ⇒
        partnerApplicationQuery(freePartner._1).map { partnerApplication ⇒
          val pPartners = partnersPartners(freePartner._1, partnerApplication)
          if (pPartners.contains(member)) {
            val partner = Individual(freePartner._1)
            i.add(lwm.hasMember, freePartner._1)
            partner.add(lwm.memberOf, groupResource)
            sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(partnerApplication))
          }
        }
      }

      i.add(lwm.hasMember, member)
      Individual(member).add(lwm.memberOf, groupResource)
      i = Individual(groupResource)
      sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(application))
    }

  }

  def group(labwork: Resource, applications: List[Resource], minGroupSize: Int = 3, maxGroupSize: Int = 15): Future[Boolean] = Future {
    var unprocessedApplications = applications

    val existingGroups = Individual(labwork).props.get(lwm.hasGroup).map { groups ⇒
      groups.map { group ⇒
        ExistingGroup(Resource(group.value))
      }
    }.fold(List.empty[ExistingGroup])(list ⇒ list)

    var additionalGroups = Vector.empty[Group]

    while (unprocessedApplications.size > 0) {
      val application = Individual(unprocessedApplications.head)
      unprocessedApplications = unprocessedApplications.tail

      val applicant = application.props.getOrElse(lwm.hasApplicant, List.empty[RDFNode]).map(_.asResource().get)
      val partners = application.props.getOrElse(lwm.hasPartner, List.empty[RDFNode]).map(_.asResource().get)

      applicant.headOption.map { a ⇒
        if (existingGroups.size > 0) {
          val minGroup = existingGroups.minBy(_.members.size)
          if (minGroup.members.size + partners.size + 1 < maxGroupSize * 1.1) {
            minGroup.addMember(Resource(a.value), application.uri, partners)
          } else {
            if (additionalGroups.size > 0) {
              val minGroup = additionalGroups.minBy(_.members.size)
              if (minGroup.members.size + partners.size + 1 < maxGroupSize * 1.1) {
                minGroup.addMember(Resource(a.value), application.uri, partners)
              } else {
                if (unprocessedApplications.size < minGroupSize) {
                  minGroup.addMember(Resource(a.value), application.uri, partners)
                } else {
                  val group = Group(labwork)
                  additionalGroups = group +: additionalGroups
                  group.addMember(Resource(a.value), application.uri, partners)
                }
              }
            } else {
              val group = Group(labwork)
              additionalGroups = group +: additionalGroups
              group.addMember(Resource(a.value), application.uri, partners)
            }
          }
        } else {
          if (additionalGroups.size > 0) {
            val minGroup = additionalGroups.minBy(_.members.size)
            if (minGroup.members.size + partners.size + 1 < maxGroupSize * 1.1) {
              minGroup.addMember(Resource(a.value), application.uri, partners)
            } else {
              if (unprocessedApplications.size < minGroupSize) {
                minGroup.addMember(Resource(a.value), application.uri, partners)
              } else {
                val group = Group(labwork)
                additionalGroups = group +: additionalGroups
                group.addMember(Resource(a.value), application.uri, partners)
              }
            }
          } else {
            val group = Group(labwork)
            additionalGroups = group +: additionalGroups
            group.addMember(Resource(a.value), application.uri, partners)
          }
        }
      }
    }
    true
  }

}

