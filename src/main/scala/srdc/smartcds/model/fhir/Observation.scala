package srdc.smartcds.model.fhir

import srdc.smartcds.util.JsonClass

case class Observation(resourceType: String,
                       id: Option[String],
                       meta: Option[Meta],
                       code: CodeableConcept,
                       status: String,
                       effectiveDateTime: Option[String],
                       category: Option[Seq[CodeableConcept]],
                       valueQuantity: Option[Quantity],
                       valueBoolean: Option[Boolean],
                       valueCodeableConcept: Option[CodeableConcept],
                       valueString: Option[String],
                       valueInteger: Option[Int],
                       interpretation: Option[Array[CodeableConcept]],
                       component: Option[Array[ObservationComponent]],
                       subject: Option[Reference],
                       basedOn: Option[Array[Reference]],
                       note: Option[Seq[Annotation]],
                       var extension: Option[Array[Extension]] = None
                      ) extends JsonClass

final case class ObservationComponent(code: CodeableConcept,
                                      valueQuantity: Option[Quantity],
                                      valueBoolean: Option[Boolean],
                                      valueCodeableConcept: Option[CodeableConcept],
                                      valueString: Option[String],
                                      valueInteger: Option[Int],
                                      dataAbsentReason: Option[CodeableConcept],
                                      interpretation: Option[Array[CodeableConcept]]) extends JsonClass
