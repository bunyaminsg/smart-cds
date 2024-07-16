package model.fhir

import util.CareTeamStatus.CareTeamStatus
import util.JsonClass

final case class CareTeam(resourceType: String,
                          status: CareTeamStatus,
                          name: Option[String],
                          priority: Option[String],
                          subject: Option[Reference],
                          participant: Option[Seq[CareTeamParticipant]]
                          ) extends JsonClass

final case class CareTeamParticipant(role: Option[Seq[CodeableConcept]],
                                     member: Option[Reference],
                                     extension: Seq[Extension]) extends JsonClass
