package cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import model.fhir.{Condition, MedicationStatement, Observation, Patient}
import util.{DateTimeUtil, FhirParseHelper}

import scala.util.Try

//noinspection ScalaDocMissingParameterDescription,DuplicatedCode
object ACCAHAFlowExecution {

  /**
   * Executes ACC/AHA calculation service
   *
   * @param patient
   * @param TotalCholesterol
   * @param HDLCholesterol
   * @param SystolicBP
   * @param SmokingStatus
   * @param Type1Diabetes
   * @param Type2Diabetes
   * @param HypertensiveTreatment
   * @param Ethnicity
   * @param responseBuilder
   * @return
   */
  def executeFlow(patient: Patient, TotalCholesterol: Seq[Observation], HDLCholesterol: Seq[Observation],
                  SystolicBP: Seq[Observation], SmokingStatus: Seq[Observation], Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition],
                  HypertensiveTreatment: Seq[MedicationStatement], Ethnicity: Seq[Observation],
                  responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {

    var output = responseBuilder
    val riskScore = calculateACCRisk(patient, TotalCholesterol, HDLCholesterol, SystolicBP, SmokingStatus, Type1Diabetes, Type2Diabetes, HypertensiveTreatment, Ethnicity)

    if (riskScore.isDefined) {
      output = output.withCard(_.loadCardWithPostTranslation("card-score",
        "effectiveDate" -> DateTimeUtil.zonedNow(),
        "scoreValue" -> riskScore.get
      ))
    }

    output
  }

  /**
   * Validates given prefetch and returns the ACC/AHA risk score
   */
  private def calculateACCRisk(patient: Patient, TotalCholesterol: Seq[Observation], HDLCholesterol: Seq[Observation],
                               SystolicBP: Seq[Observation], SmokingStatus: Seq[Observation], Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition],
                               HypertensiveTreatment: Seq[MedicationStatement], Ethnicity: Seq[Observation]): Option[Double] = {
    val checkExists = (resources: Seq[Any]) => if (resources.nonEmpty) 1 else 0

    val age = FhirParseHelper.getAge(patient)
    val gender = patient.gender
    val race = determineRace(Ethnicity)
    val diabetes = checkExists(Type1Diabetes) | checkExists(Type2Diabetes)
    val treatedHypertension = checkExists(HypertensiveTreatment)

    val totalCholesterolOpt = Try(TotalCholesterol.head.valueQuantity.get.value.get).toOption
    val hdlCholesterolOpt = Try(HDLCholesterol.head.valueQuantity.get.value.get).toOption
    val systolicBPOpt = Try(SystolicBP.head.valueQuantity.get.value.get).toOption
    val smokingObs = SmokingStatus.headOption

    if (totalCholesterolOpt.isEmpty || hdlCholesterolOpt.isEmpty || systolicBPOpt.isEmpty || smokingObs.isEmpty) {
      println("Total Cholesterol, HDL Cholesterol, Systolic BP or Smoking Status not found")
      return None
    }

    val totalCholesterol = totalCholesterolOpt.get
    val hdlCholesterol = hdlCholesterolOpt.get
    val sbp = systolicBPOpt.get

    val smoking = if (smokingObs.isDefined) {
      smokingObs.get.valueCodeableConcept.get.coding.map(_.code).toSeq
    } else {
      Seq("266919005")
    }
    val smoker = if (Seq("LA18976-3", "LA18977-1", "LA18981-3", "LA18982-1", "LA18979-7").intersect(smoking).nonEmpty) 1 else 0

    if (gender.contains("male")) {
      Some(calculateACCRiskM(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race))
    } else {
      Some(calculateACCRiskF(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race))
    }
  }

  /**
   * Determines the race of the patient based on Ethnicity observation
   */
  private def determineRace(ethnicity: Seq[Observation]): String = {
    val ethnicityCodes = ethnicity.flatMap(_.valueCodeableConcept.toSeq).flatMap(_.coding.map(_.code))
    if (ethnicityCodes.contains("46463-6")) "africanamerican" else "white"
  }

  /**
   * Calculate ACC/AHA risk score for male patients
   */
  private def calculateACCRiskM(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Int, diabetes: Int, treatedHypertension: Int, race: String): Double = {

    val lnAge = math.log(age)
    val lnTotalCholesterol = math.log(totalCholesterol)
    val lnHDLCholesterol = math.log(hdlCholesterol)
    val lnSBP = math.log(sbp)

    val (coefLnAge, coefLnAgeSquared, coefLnTotalCholesterol, coefLnAgeLnTotalCholesterol, coefLnHDLCholesterol, coefLnAgeLnHDLCholesterol, coefLnTreatedSBP, coefLnAgeLnTreatedSBP, coefLnUntreatedSBP, coefLnAgeLnUntreatedSBP, smokerCoefficient, coefLnAgeSmoker, diabetesCoefficient, baselineSurvival) = race match {
      case "africanamerican" => (
        2.469, // lnAge
        0.0, // lnAgeSquared
        0.302, // lnTotalCholesterol
        0.0, // lnAge * lnTotalCholesterol
        -0.307, // lnHDLCholesterol
        0, // lnAge * lnHDLCholesterol
        1.916, // lnTreatedSBP
        0, // lnAge * lnTreatedSBP
        1.809, // lnUntreatedSBP
        0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 0.549 else 0.0, // smoker
        0, // lnAge * smoker
        if (diabetes == 1) 0.645 else 0.0, // diabetes
        0.9533 // baselineSurvival
      )
      case _ => (
        12.344, // lnAge
        0, // lnAgeSquared
        11.853, // lnTotalCholesterol
        -2.664, // lnAge * lnTotalCholesterol
        -7.990, // lnHDLCholesterol
        1.769, // lnAge * lnHDLCholesterol
        1.797, // lnTreatedSBP
        0, // lnAge * lnTreatedSBP
        1.764, // lnUntreatedSBP
        0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 7.837 else 0.0, // smoker
        -1.795, // lnAge * smoker
        if (diabetes == 1) 0.658 else 0.0, // diabetes
        0.9665 // baselineSurvival
      )
    }

    val lnSum = coefLnAge * lnAge +
      coefLnAgeSquared * math.pow(lnAge, 2) +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      (if (treatedHypertension == 1) coefLnAgeLnTreatedSBP else coefLnAgeLnUntreatedSBP) * lnAge * lnSBP +
      smokerCoefficient +
      coefLnAgeSmoker * lnAge * smoker +
      diabetesCoefficient

    100.0 * (1 - math.pow(baselineSurvival, math.exp(lnSum)))
  }

  /**
   * Calculate ACC/AHA risk score for female patients
   */
  private def calculateACCRiskF(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Int, diabetes: Int, treatedHypertension: Int, race: String): Double = {

    val lnAge = math.log(age)
    val lnTotalCholesterol = math.log(totalCholesterol)
    val lnHDLCholesterol = math.log(hdlCholesterol)
    val lnSBP = math.log(sbp)

    val (coefLnAge, coefLnAgeSquared, coefLnTotalCholesterol, coefLnAgeLnTotalCholesterol, coefLnHDLCholesterol, coefLnAgeLnHDLCholesterol, coefLnTreatedSBP, coefLnAgeLnTreatedSBP, coefLnUntreatedSBP, coefLnAgeLnUntreatedSBP, smokerCoefficient, coefLnAgeSmoker, diabetesCoefficient, baselineSurvival) = race match {
      case "africanamerican" => (
        17.114, // lnAge
        0.0, // lnAgeSquared
        0.940, // lnTotalCholesterol
        0.0, // lnAge * lnTotalCholesterol
        -18.920, // lnHDLCholesterol
        4.475, // lnAge * lnHDLCholesterol
        29.291, // lnTreatedSBP
        -6.432, // lnAge * lnTreatedSBP
        27.820, // lnUntreatedSBP
        -6.087, // lnAge * lnUntreatedSBP
        if (smoker == 1) 0.691 else 0.0, // smoker
        0, // lnAge * smoker
        if (diabetes == 1) 0.874 else 0.0, // diabetes
        0.9533 // baselineSurvival
      )
      case _ => (
        -29.799, // lnAge
        4.884, // lnAgeSquared
        13.540, // lnTotalCholesterol
        -3.114, // lnAge * lnTotalCholesterol
        -13.578, // lnHDLCholesterol
        3.149, // lnAge * lnHDLCholesterol
        2.019, // lnTreatedSBP
        0, // lnAge * lnTreatedSBP
        1.957, // lnUntreatedSBP
        0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 7.574 else 0.0, // smoker
        -1.665, // lnAge * smoker
        if (diabetes == 1) 0.661 else 0.0, // diabetes
        0.9665 // baselineSurvival
      )
    }

    val lnSum = coefLnAge * lnAge +
      coefLnAgeSquared * math.pow(lnAge, 2) +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      (if (treatedHypertension == 1) coefLnAgeLnTreatedSBP else coefLnAgeLnUntreatedSBP) * lnAge * lnSBP +
      smokerCoefficient +
      coefLnAgeSmoker * lnAge * smoker +
      diabetesCoefficient

    100.0 * (1 - math.pow(baselineSurvival, math.exp(lnSum)))
  }
}
