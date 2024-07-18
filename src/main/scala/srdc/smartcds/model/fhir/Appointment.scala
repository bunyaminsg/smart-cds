package srdc.smartcds.model.fhir

import srdc.smartcds.util.JsonClass

case class AppointmentParticipant(actor: Reference,
                                  status: String)

case class Appointment(resourceType: String,
                       id: Option[String],
                       meta: Option[Meta],
                       status: String,
                       serviceType: Option[Array[CodeableConcept]],
                       appointmentType: Option[Array[CodeableConcept]],
                       start: Option[String],
                       end: Option[String],
                       var basedOn: Option[Array[Reference]],
                       participant: Option[Array[AppointmentParticipant]],
                       extension: Option[Extension],
                       comment: Option[String]) extends JsonClass
