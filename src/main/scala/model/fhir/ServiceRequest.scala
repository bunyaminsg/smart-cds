package model.fhir

import util.CarePlanIntent.CarePlanIntent
import util.JsonClass

case class ServiceRequest(resourceType: String, id: Option[String],
                          meta: Option[Meta],
                          status: String, category: Option[Array[CodeableConcept]], subject: Reference,
                          code: Option[CodeableConcept], performerType: Option[CodeableConcept],
                          occurrenceTiming: Option[OccurrenceTiming], occurrenceDateTime: Option[String],
                          occurrencePeriod: Option[OccurrencePeriod], var basedOn: Option[Array[Reference]],
                          intent: CarePlanIntent, requester: Option[Reference], performer: Option[Array[Reference]],
                          note: Option[Seq[Annotation]], extension: Option[Array[Extension]]) extends JsonClass
