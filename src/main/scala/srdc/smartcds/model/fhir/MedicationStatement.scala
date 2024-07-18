package srdc.smartcds.model.fhir

import scala.util.Try

case class MedicationStatement(
                              status: String,
                              medicationCodeableConcept: CodeableConcept,
                              effectivePeriod: Option[Period],
                              dateAsserted: Option[String],
                              dosage: Option[Seq[Dosage]]
                              ) {

  def getDateAsserted: String = {
    Try(dateAsserted.get).getOrElse("")
  }

  def getStartDate: String = {
    effectivePeriod.flatMap(_.start).getOrElse(dateAsserted.getOrElse(""))
  }
}

final case class Dosage(
                  doseAndRate: Option[Seq[DosageAndRate]]
                  )

final case class DosageAndRate(
                         doseQuantity: Option[Quantity]
                         )