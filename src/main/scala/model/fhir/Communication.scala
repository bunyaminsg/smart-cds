package model.fhir

import util.EventStatus.EventStatus
import util.JsonClass

final case class Communication(resourceType: String,
                               id: Option[String],
                               status: EventStatus,
                               category: Option[Seq[CodeableConcept]],
                               priority: Option[String],
                               medium: Option[Seq[CodeableConcept]],
                               subject: Option[Reference],
                               topic: Option[CodeableConcept],
                               about: Option[Seq[Reference]],
                               sent: Option[String],
                               recipient: Option[Seq[Reference]],
                               sender: Option[Reference],
                               payload:Option[Seq[CommunicationPayload]],
                               extension: Option[Array[Extension]]
                              ) extends JsonClass

final case class CommunicationPayload(contentString: Option[String],
                                      contentReference: Option[Reference]) extends JsonClass
