package model.fhir

case class DosageInstruction(text: Option[String], timing: Option[OccurrenceTiming], doseAndRate: Option[Seq[DosageAndRate]])

case class MedicationRequest(id: Option[String], status: String, medicationCodeableConcept: CodeableConcept,
                             dosageInstruction: Option[Array[DosageInstruction]], subject: Option[Reference])
