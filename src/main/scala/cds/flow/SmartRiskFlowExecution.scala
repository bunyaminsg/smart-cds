package cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import model.fhir._
import util._

import scala.math.{log, pow}

object SmartRiskFlowExecution {

  /**
   * Executes QRISK calculation service
   * @param patient
   * @param AtrialFibrillation
   * @param RheumatoidArthritis
   * @param CKD4_5
   * @param Type1Diabetes
   * @param Type2Diabetes
   * @param HypertensiveTreatment
   * @param BMI
   * @param TotalCholesterol
   * @param HDL
   * @param BP_SBP
   * @param SmokingStatus
   * @param CVD_FMH
   * @param responseBuilder
   * @return
   */
  def executeFlow(patient: Patient, Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition], CerebrovascularDisease: Seq[Condition],
                  AcuteCoronarySyndrome: Seq[Condition], ChronicCoronarySyndromes: Seq[Condition], AorticAneurysm: Seq[Condition],
                  PeripheralArteryDisease: Seq[Condition], TotalCholesterol: Seq[Observation], HDL: Seq[Observation],
                  BP_SBP: Seq[Observation], SmokingStatus: Seq[Observation], CRP: Seq[Observation], Egfr: Seq[Observation], responseBuilder: CdsResponseBuilder) = {
    var output = responseBuilder
    val age: Double = FhirParseHelper.getAge(patient)
    val isSmoking = SmokingStatus.headOption.exists(_.valueCodeableConcept.exists(_.coding.exists(_.code match {
      case "LA18976-3" | "LA18979-7" | "LA18977-1" | "LA18982-1" | "LA18981-3" => true
      case _ => false
    })))
    val sbp = FhirParseHelper.getSystolicBP(BP_SBP)
    val cholesterol = FhirParseHelper.getQuantityObservationValue(TotalCholesterol.headOption, Some(UnitConceptEnum.CHOLESTEROL))
    val hdl = FhirParseHelper.getQuantityObservationValue(HDL.headOption, Some(UnitConceptEnum.CHOLESTEROL))
    val crp = FhirParseHelper.getQuantityObservationValue(CRP.headOption, None)
    val egfr = FhirParseHelper.getQuantityObservationValue(Egfr.headOption, None)

    var riskScore: Double = 0

    // TODO: implement the SMART algorithm

    output = output.withCard(_.loadCardWithPostTranslation("card-score",
      "effectiveDate" -> DateTimeUtil.zonedNow(),
      "scoreValue" -> riskScore
    ))

    output
  }
}
