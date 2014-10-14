package utils

import models.{ LabWorkGroup, LabworkGroups }
import utils.Global._
import utils.semantic.Vocabulary.LWM
import utils.semantic._

import scala.concurrent.Future

object ListGrouping {

  import scala.concurrent.ExecutionContext.Implicits.global

  case class Group(labwork: Resource) {
    var members = Vector.empty[Resource]

    val labworkIndividual = Individual(labwork)
    val id = labworkIndividual.props.getOrElse(LWM.hasGroup, List.empty[RDFNode]).size + 'A'
    val labworkGroup = LabWorkGroup(id.toChar.toString, labwork)
    val groupIndividual = LabworkGroups.create(labworkGroup)

    def addMember(member: Resource, application: Resource, partners: List[Resource]) = {
      def partnerGroupQuery(partner: Resource) = {
        s"""
           select ($partner as ?s) (${LWM.memberOf} as ?p) (?group as ?o) where {
            $application ${LWM.hasLabWork} ?labwork .
            ?labwork ${LWM.hasGroup} ?group .
            $partner ${LWM.memberOf} ?group .
            }
         """.stripMargin
      }

      def partnerApplicationQuery(partner: Resource) = {
        val query = s"""
           select ($partner as ?s) (${LWM.hasPendingApplication} as ?p) (?application as ?o) where {
            $application ${LWM.hasLabWork} ?labwork .
            $partner ${LWM.hasPendingApplication} ?application .
            ?application ${LWM.hasLabWork} ?labwork .
            }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      def partnersPartners(partner: Resource, application: Resource) = {
        val query = s"""
           select ($partner as ?s) (${LWM.hasPartner} as ?p) (?partner as ?o) where {
            $application ${LWM.hasPartner} ?partner
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
              i.add(LWM.hasMember, freePartner._1)
              members = freePartner._1 +: members
              partner.add(LWM.memberOf, i.uri)
            }
            sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(partnerApplication))
          }
        }
      }

      groupIndividual.map { i ⇒
        i.add(LWM.hasMember, member)
        members = member +: members
        Individual(member).add(LWM.memberOf, i.uri)
      }

      sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(application))
    }

  }

  case class ExistingGroup(groupResource: Resource) {

    var i = Individual(groupResource)

    def members = i.props.get(LWM.hasMember).fold(List.empty[RDFNode])(list ⇒ list)

    def addMember(member: Resource, application: Resource, partners: List[Resource]) = {
      def partnerGroupQuery(partner: Resource) = {
        s"""
           select ($partner as ?s) (${LWM.memberOf} as ?p) (?group as ?o) where {
            $application ${LWM.hasLabWork} ?labwork .
            ?labwork ${LWM.hasGroup} ?group .
            $partner ${LWM.memberOf} ?group .
            }
         """.stripMargin
      }

      def partnerApplicationQuery(partner: Resource) = {
        val query = s"""
           select ($partner as ?s) (${LWM.hasPendingApplication} as ?p) (?application as ?o) where {
            $application ${LWM.hasLabWork} ?labwork .
            $partner ${LWM.hasPendingApplication} ?application .
            ?application ${LWM.hasLabWork} ?labwork .
            }
         """.stripMargin

        val result = sparqlExecutionContext.executeQueryBlocking(query)
        SPARQLTools.statementsFromString(result).headOption.map(_.o.asResource().get)
      }

      def partnersPartners(partner: Resource, application: Resource) = {
        val query = s"""
           select ($partner as ?s) (${LWM.hasPartner} as ?p) (?partner as ?o) where {
            $application ${LWM.hasPartner} ?partner
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
            i.add(LWM.hasMember, freePartner._1)
            partner.add(LWM.memberOf, groupResource)
            sparqlExecutionContext.executeUpdate(SPARQLBuilder.removeIndividual(partnerApplication))
          }
        }
      }

      i.add(LWM.hasMember, member)
      Individual(member).add(LWM.memberOf, groupResource)
      i = Individual(groupResource)
      sparqlExecutionContext.executeUpdateBlocking(SPARQLBuilder.removeIndividual(application))
    }

  }

  def group(labwork: Resource, applications: List[Resource], minGroupSize: Int = 3, maxGroupSize: Int = 15): Future[Boolean] = Future {
    var unprocessedApplications = applications

    val existingGroups = Individual(labwork).props.get(LWM.hasGroup).map { groups ⇒
      groups.map { group ⇒
        ExistingGroup(Resource(group.value))
      }
    }.fold(List.empty[ExistingGroup])(list ⇒ list)

    var additionalGroups = Vector.empty[Group]

    while (unprocessedApplications.size > 0) {
      val application = Individual(unprocessedApplications.head)
      unprocessedApplications = unprocessedApplications.tail

      val applicant = application.props.getOrElse(LWM.hasApplicant, List.empty[RDFNode]).map(_.asResource().get)
      val partners = application.props.getOrElse(LWM.hasPartner, List.empty[RDFNode]).map(_.asResource().get)

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

