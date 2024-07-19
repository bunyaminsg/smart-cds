package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir.{Condition, MedicationStatement, Observation, Patient}
import srdc.smartcds.util.{DateTimeUtil, FhirParseHelper}

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
    val riskScores = calculateACCRisk(patient, TotalCholesterol, HDLCholesterol, SystolicBP, SmokingStatus, Type1Diabetes, Type2Diabetes, HypertensiveTreatment, Ethnicity)

    riskScores match {
      case Some((patientScore, healthyScore)) =>
        output = output.withCard(_.loadCardWithPostTranslation("card-score",
          "effectiveDate" -> DateTimeUtil.zonedNow(),
          "patientScoreValue" -> patientScore,
          "healthyScoreValue" -> healthyScore
        ))
      case None =>
        println("Unable to calculate ACC/AHA risk score due to missing or incomplete data.")
    }

    output
  }

  /**
   * Validates given prefetch and returns the ACC/AHA risk score
   */
  private def calculateACCRisk(patient: Patient, TotalCholesterol: Seq[Observation], HDLCholesterol: Seq[Observation],
                               SystolicBP: Seq[Observation], SmokingStatus: Seq[Observation], Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition],
                               HypertensiveTreatment: Seq[MedicationStatement], Ethnicity: Seq[Observation]): Option[(Double, Double)] = {

    def checkExists(resources: Seq[Any]): Int = if (resources.nonEmpty) 1 else 0

    val age = FhirParseHelper.getAge(patient)
    val gender = patient.gender
    val race = determineRace(Ethnicity)
    val diabetes = checkExists(Type1Diabetes) | checkExists(Type2Diabetes)
    val treatedHypertension = checkExists(HypertensiveTreatment)

    val totalCholesterolOpt = Try(TotalCholesterol.head.valueQuantity.get.value.get).toOption
    val hdlCholesterolOpt = Try(HDLCholesterol.head.valueQuantity.get.value.get).toOption
    val systolicBPOpt = FhirParseHelper.getSystolicBP(SystolicBP)
    val smokingObs = SmokingStatus.headOption

    if (totalCholesterolOpt.isEmpty) {
      println("Total Cholesterol not found")
      return None
    }

    if (hdlCholesterolOpt.isEmpty) {
      println("HDL not found")
      return None
    }

    if (systolicBPOpt.isEmpty) {
      println("SBP not found")
      return None
    }

    if (smokingObs.isEmpty) {
      println("smoking not found")
      return None
    }

    val totalCholesterol = totalCholesterolOpt.get
    val hdlCholesterol = hdlCholesterolOpt.get
    val sbp = systolicBPOpt.get

    println(s"Calculating ACC Risk with values: age=$age, totalCholesterol=$totalCholesterol, hdlCholesterol=$hdlCholesterol, sbp=$sbp, diabetes=$diabetes, treatedHypertension=$treatedHypertension, race=$race")

    val smoking = smokingObs.map(_.valueCodeableConcept.get.coding.map(_.code).toSeq).getOrElse(Seq("266919005"))
    val smoker = if (Seq("LA18976-3", "LA18977-1", "LA18981-3", "LA18982-1", "LA18979-7").intersect(smoking).nonEmpty) 1 else 0

    gender match {
      case Some("male") =>
        val patientScore = calculateACCRiskM(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race)
        val healthyScore = calculateACCRiskM(age, 170, 50, 110, 0, 0, 0, race)
        println(s"Calculated scores: patientScore=$patientScore, healthyScore=$healthyScore")
        Some(patientScore, healthyScore)
      case Some("female") =>
        val patientScore = calculateACCRiskF(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race)
        val healthyScore = calculateACCRiskF(age, 170, 50, 110, 0, 0, 0, race)
        println(s"Calculated scores: patientScore=$patientScore, healthyScore=$healthyScore")
        Some(patientScore, healthyScore)
      case _ =>
        println("Gender not specified or invalid")
        None
    }
  }

  /**
   * Determines the race of the patient based on Ethnicity observation
   */
  private def determineRace(ethnicity: Seq[Observation]): String = {
    val blackEthnicityCodes = Seq("LA6162-7") // Black or African-American LOINC code
    val ethnicityCodes = ethnicity.flatMap(_.valueCodeableConcept.toSeq).flatMap(_.coding.map(_.code))

    if (blackEthnicityCodes.intersect(ethnicityCodes).nonEmpty) "africanamerican" else "white"
  }

  /**
   * Calculate ACC/AHA risk score for male patients
   */
  private def calculateACCRiskM(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Int,
                                diabetes: Int, treatedHypertension: Int, race: String): Double = {
    val lnAge = math.log(age)
    val lnTotalCholesterol = math.log(totalCholesterol)
    val lnHDLCholesterol = math.log(hdlCholesterol)
    val lnSBP = math.log(sbp)

    // Coefficients for white males as per ACC/AHA guidelines
    val intercept = -29.799
    val coefLnAge = 12.344
    val coefLnTotalCholesterol = 11.853
    val coefLnAgeLnTotalCholesterol = -2.664
    val coefLnHDLCholesterol = -7.990
    val coefLnAgeLnHDLCholesterol = 1.769
    val coefLnTreatedSBP = 1.797
    val coefLnUntreatedSBP = 1.764
    val coefSmoker = if (smoker == 1) 7.837 else 0.0
    val coefLnAgeSmoker = -1.795
    val coefDiabetes = if (diabetes == 1) 0.658 else 0.0
    val baselineSurvival = 0.9665

    // Calculate lnSum
    val lnSum = intercept +
      coefLnAge * lnAge +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      coefSmoker +
      coefLnAgeSmoker * lnAge * smoker +
      coefDiabetes

    println(s"lnSum: $lnSum")

    // Calculate risk score
    val riskScore = 100.0 * (1 - math.pow(baselineSurvival, math.exp(lnSum)))
    println(s"Calculated ACC/AHA risk score (male): $riskScore")

    riskScore
  }


  /**
   * Calculate ACC/AHA risk score for female patients
   */
  private def calculateACCRiskF(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Int,
                                diabetes: Int, treatedHypertension: Int, race: String): Double = {
    val lnAge = math.log(age)
    val lnTotalCholesterol = math.log(totalCholesterol)
    val lnHDLCholesterol = math.log(hdlCholesterol)
    val lnSBP = math.log(sbp)

    // Coefficients for white females as per ACC/AHA guidelines
    val intercept = -29.799
    val coefLnAge = 12.344
    val coefLnTotalCholesterol = 11.853
    val coefLnAgeLnTotalCholesterol = -2.664
    val coefLnHDLCholesterol = -7.990
    val coefLnAgeLnHDLCholesterol = 1.769
    val coefLnTreatedSBP = 1.797
    val coefLnUntreatedSBP = 1.764
    val coefSmoker = if (smoker == 1) 7.837 else 0.0
    val coefLnAgeSmoker = -1.795
    val coefDiabetes = if (diabetes == 1) 0.658 else 0.0
    val baselineSurvival = 0.9665

    // Calculate lnSum
    val lnSum = intercept +
      coefLnAge * lnAge +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      coefSmoker +
      coefLnAgeSmoker * lnAge * smoker +
      coefDiabetes

    println(s"lnSum: $lnSum")

    // Calculate risk score
    val riskScore = 100.0 * (1 - math.pow(baselineSurvival, math.exp(lnSum)))
    println(s"Calculated ACC/AHA risk score (female): $riskScore")

    riskScore
  }
}
