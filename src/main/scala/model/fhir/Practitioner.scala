package model.fhir

case class Practitioner(resourceType: String, id: Option[String], active: Option[Boolean],
                        birthDate: Option[String], gender: Option[String],
                        qualification: Option[Array[PractitionerQualification]],
                        address: Option[Array[Address]], name: Option[Array[HumanName]],
                        identifier: Option[Array[Identifier]], telecom: Option[Array[ContactPoint]])

case class PractitionerQualification(identifier: Option[Array[Identifier]], code: CodeableConcept, period: Option[Period], issuer: Option[Reference])