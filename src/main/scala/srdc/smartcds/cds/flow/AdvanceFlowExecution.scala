package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir._
import srdc.smartcds.util.{DateTimeUtil, FhirParseHelper, UnitConceptEnum}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Period}

object AdvanceFlowExecution {
  /**
   * Execute ADVANCE Calculation flow
   *
   * @param patient Patient resource
   * @param AtrialFibrillation Atrial Fibrillation Condition
   * @param DiabeticRetinopathy Diabetic Retinopathy Condition
   * @param HypertensiveTreatment Hypertensive Treatment Medication
   * @param HbA1C HbA1C Observation
   * @param ACR Albumin/Creatine Ratio Observation
   * @param TotalCholesterol Total Cholesterol Observation
   * @param HDL HDL Observation
   * @param NonHDL NonHDL Observation
   * @param BP_SBP Systolic Blood Pressure Observation
   * @param BP_DBP Diastolic Blood Pressure Observation
   * @param Type1Diabetes Type 1 Diabetes Condition
   * @param Type2Diabetes Type 2 Diabetes Condition
   * @param responseBuilder Response Builder
   * @return
   */
  def executionFlow(patient: Patient, AtrialFibrillation: Seq[Condition],DiabeticRetinopathy: Seq[Observation], HypertensiveTreatment: Seq[MedicationStatement],
                    HbA1C: Seq[Observation], ACR: Seq[Observation], TotalCholesterol: Seq[Observation], HDL: Seq[Observation], NonHDL: Seq[Observation],
                    BP_SBP: Seq[Observation], BP_DBP: Seq[Observation], Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition], responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {
    val checkExists = (resources: Seq[Any]) => if (resources.nonEmpty) 1 else 0

    val age = FhirParseHelper.getAge(patient)
    val isFemale = patient.gender.contains("female")
    val sbpOpt = FhirParseHelper.getSystolicBP(BP_SBP)
    val dbpOpt = FhirParseHelper.getDiastolicBP(BP_DBP)

    var nonhdl = 0.0
    if(checkExists(NonHDL) == 1){
      val nonhdlObs = NonHDL.headOption
      nonhdl = FhirParseHelper.getQuantityObservationValue(nonhdlObs, Option(UnitConceptEnum.CHOLESTEROL)).get
    } else if(checkExists(HDL)==1 && checkExists(TotalCholesterol) == 1){
      val hdlObs = HDL.headOption
      val cholesterolObs = TotalCholesterol.headOption
      val cholesterol = FhirParseHelper.getQuantityObservationValue(cholesterolObs, Option(UnitConceptEnum.CHOLESTEROL)).get
      val hdl = FhirParseHelper.getQuantityObservationValue(hdlObs, Option(UnitConceptEnum.CHOLESTEROL)).get
      nonhdl = cholesterol - hdl
    } else {
      nonhdl = 3.5
    }

    var acr = 0.0
    if(checkExists(ACR) == 1){
      val acrObs = ACR.headOption
      acr = FhirParseHelper.getQuantityObservationValue(acrObs, Option(UnitConceptEnum.ACR)).get
    } else {acr = 20.0}

    var hba1c = 0.0
    if(checkExists(HbA1C) == 1){
      val hba1cObs = HbA1C.headOption
      hba1c = FhirParseHelper.getQuantityObservationValue(hba1cObs, Option(UnitConceptEnum.HBA1C)).get
    } else {hba1c = 5.0}

    val sbp = sbpOpt.get
    val dbp = dbpOpt.get

    val b_AF = checkExists(AtrialFibrillation)
    val b_treatedhyp = checkExists(HypertensiveTreatment)
    val b_DR = checkExists(DiabeticRetinopathy)


    var diabetes_duration = -1
    if(checkExists(Type1Diabetes) == 1){
      diabetes_duration = Period.between(LocalDate.parse(Type1Diabetes.head.recordedDate.get, DateTimeFormatter.ofPattern("yyyy-MM-dd")), LocalDate.now()).getYears
    } else if(checkExists(Type2Diabetes) == 1) {
      diabetes_duration = Period.between(LocalDate.parse(Type2Diabetes.head.recordedDate.get, DateTimeFormatter.ofPattern("yyyy-MM-dd")), LocalDate.now()).getYears
    } else {
      println("Person is not diabetic")
      None
    }
    val diabetes_age = age + diabetes_duration
    if (sbpOpt.isEmpty|| dbpOpt.isEmpty) {
      println("SBP, DBP, ACR, HbA1C or NonHDL not found")
      None
    }


    val pulse_p = sbp - dbp
    var sum = 0
    var healthy_sum = 0
    var score = 0
    if(diabetes_age<=34) {
      score = 0
    } else {
        score = diabetes_age match {
        case x if 35 to 39 contains x => 1
        case x if 40 to 44 contains x => 2
        case x if 45 to 50 contains x => 3
        case x if 51 to 56 contains x => 4
        case x if 57 to 62 contains x => 5
        case x if 63 to 68 contains x => 6
        case x if 68 to 74 contains x => 7
        case x if 75 to 80 contains x => 8
        case _ => 9
      }
    }
    sum += score
    healthy_sum += score

    if(diabetes_duration < 1){
      score = 0
    } else {
      score = diabetes_duration match {
        case x if 1 to 5 contains x => 1
        case x if 6 to 10 contains x => 2
        case x if 11 to 15 contains x => 3
        case x if 16 to 20 contains x => 4
        case x if 21 to 25 contains x => 5
        case x if 26 to 30 contains x => 6
        case x if 31 to 36 contains x => 7
        case _ => 8
      }
    }
    sum += score
    healthy_sum += score


    if(isFemale) {
      sum -= 1
      healthy_sum -= 1
    }
    if(pulse_p > 110) {
      sum += 2
    } else if(pulse_p >= 50) {
      sum += 1
    }
    if(b_DR == 1) {sum += 1}
    if(b_treatedhyp == 1) {sum += 1}
    if(b_AF == 1) {sum += 2}
    if(hba1c >= 9) {
      sum += 2
    } else if(hba1c >= 6) {
      sum += 1
    }
    if(nonhdl>=9) {
      sum += 5
    } else if(nonhdl >= 6){
      sum += 2
    } else if(nonhdl >= 3){
      sum += 1
    }
    if(acr>300) {
      sum += 3
    } else if(acr >= 30){
      sum +=2
    }

    val result = mapResult(sum)
    val healthy_score = mapResult(healthy_sum)

    var output = responseBuilder
    output = output.withCard(_.loadCardWithPostTranslation("card-score",
      "effectiveDate" -> DateTimeUtil.zonedNow(),
      "scoreValue" -> result,
      "healthyValue" -> healthy_score
    ))
    println(output)
    output

  }

  /**
   * Maps the Accumulated Score to Cardiovascular Disease Risk
   *
   * @param sum Accumulated Score
   * @return Cardiovascular Disease Risk
   */
  def mapResult(sum: Int): Double = {
    var result = 0.0
    if(sum <= 5) {
      result = 0.0
    } else {
      result = sum match{
        case 6 => 0.5
        case 7 => 0.7
        case 8 => 1.0
        case 9 => 1.4
        case 10 => 2.1
        case 11 => 3.0
        case 12 => 4.3
        case 13 => 6.2
        case 14 => 8.9
        case 15 => 12.6
        case 16 => 17.8
        case 17 => 24.7
        case 18 => 33.7
        case 19 => 41.9
        case 20 => 57.8
        case 21 => 71.4
        case _ => 83
      }
    }
    result
  }
}
