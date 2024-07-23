package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir.{Condition, MedicationStatement, Observation, Patient}
import srdc.smartcds.util.{DateTimeUtil, FhirParseHelper}

import scala.math.{exp, log, pow}
import scala.util.Try

//noinspection ScalaDocMissingParameterDescription
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

    val smoking = if (smokingObs.isDefined) {
      smokingObs.get.valueCodeableConcept.get.coding.map(_.code).toSeq
    } else {
      Seq("266919005")
    }
    val smoke_cat = if (Seq("LA18978-9", "LA18980-5", "266919005").intersect(smoking).nonEmpty) 0
    else if (smoking.contains("LA15920-4", "8517006")) 1
    else if (Seq("LA18977-1", "LA18982-1").intersect(smoking).nonEmpty) 2
    else if (Seq("LA18979-7", "LA18976-3", "449868002").intersect(smoking).nonEmpty) 3
    else if (smoking.contains("LA18981-3")) 4
    else 0

    val smoker = if (smoke_cat == 0 | smoke_cat == 1) 0 else 1

    gender match {
      case Some("male") =>
        val patientScore = calculateACCRiskM(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race)
        val healthyScore = calculateACCRiskM(age, 170, 50, 110, 0, 0, 0, race)
        Some(patientScore, healthyScore)
      case Some("female") =>
        val patientScore = calculateACCRiskF(age, totalCholesterol, hdlCholesterol, sbp, smoker, diabetes, treatedHypertension, race)
        val healthyScore = calculateACCRiskF(age, 170, 50, 110, 0, 0, 0, race)
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
  private def calculateACCRiskM(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Double,
                                diabetes: Int, treatedHypertension: Int, race: String): Double = {

    println(s"Calculating ACC Risk for male with values: age=$age, totalCholesterol=$totalCholesterol," +
      s"hdlCholesterol=$hdlCholesterol, sbp=$sbp, smoker=$smoker, diabetes=$diabetes, treatedHypertension=$treatedHypertension, race=$race")

    val lnAge = log(age)
    val lnTotalCholesterol = log(totalCholesterol)
    val lnHDLCholesterol = log(hdlCholesterol)
    val lnSBP = log(sbp)

    val (coefLnAge, coefLnAgeSquared, coefLnTotalCholesterol, coefLnAgeLnTotalCholesterol, coefLnHDLCholesterol,
    coefLnAgeLnHDLCholesterol, coefLnTreatedSBP, coefLnAgeLnTreatedSBP, coefLnUntreatedSBP, coefLnAgeLnUntreatedSBP,
    smokerCoefficient, coefLnAgeSmoker, diabetesCoefficient, baselineSurvival, mean) = race match {
      case "africanamerican" => (
        2.469, // lnAge
        0.0, // lnAgeSquared
        0.302, // lnTotalCholesterol
        0.0, // lnAge * lnTotalCholesterol
        -0.307, // lnHDLCholesterol
        0.0, // lnAge * lnHDLCholesterol
        1.916, // lnTreatedSBP
        0.0, // lnAge * lnTreatedSBP
        1.809, // lnUntreatedSBP
        0.0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 0.549 else 0.0, // smoker
        0.0, // lnAge * smoker
        if (diabetes == 1) 0.645 else 0.0, // diabetes
        0.8954, // baselineSurvival
        19.54 // mean
      )
      case _ => (
        12.344, // lnAge
        0.0, // lnAgeSquared
        11.853, // lnTotalCholesterol
        -2.664, // lnAge * lnTotalCholesterol
        -7.990, // lnHDLCholesterol
        1.769, // lnAge * lnHDLCholesterol
        1.797, // lnTreatedSBP
        0.0, // lnAge * lnTreatedSBP
        1.764, // lnUntreatedSBP
        0.0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 7.837 else 0.0, // smoker
        -1.795, // lnAge * smoker
        if (diabetes == 1) 0.658 else 0.0, // diabetes
        0.9144, // baselineSurvival
        61.18 // mean
      )
    }

    val lnSum = coefLnAge * lnAge +
      coefLnAgeSquared * pow(lnAge, 2) +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      (if (treatedHypertension == 1) coefLnAgeLnTreatedSBP else coefLnAgeLnUntreatedSBP) * lnAge * lnSBP +
      smokerCoefficient +
      coefLnAgeSmoker * lnAge * smoker +
      diabetesCoefficient

    100 * (1 - pow(baselineSurvival, exp(lnSum - mean)))
  }

  /**
   * Calculate ACC/AHA risk score for female patients
   */
  private def calculateACCRiskF(age: Int, totalCholesterol: Double, hdlCholesterol: Double, sbp: Double, smoker: Double,
                                diabetes: Int, treatedHypertension: Int, race: String): Double = {

    println(s"Calculating ACC Risk for female with values: age=$age, totalCholesterol=$totalCholesterol," +
      s"hdlCholesterol=$hdlCholesterol, sbp=$sbp, smoker=$smoker, diabetes=$diabetes," +
      s"treatedHypertension=$treatedHypertension, race=$race")

    val lnAge = log(age)
    val lnTotalCholesterol = log(totalCholesterol)
    val lnHDLCholesterol = log(hdlCholesterol)
    val lnSBP = log(sbp)

    val (coefLnAge, coefLnAgeSquared, coefLnTotalCholesterol, coefLnAgeLnTotalCholesterol, coefLnHDLCholesterol,
    coefLnAgeLnHDLCholesterol, coefLnTreatedSBP, coefLnAgeLnTreatedSBP, coefLnUntreatedSBP, coefLnAgeLnUntreatedSBP,
    smokerCoefficient, coefLnAgeSmoker, diabetesCoefficient, baselineSurvival, mean) = race match {
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
        0.0, // lnAge * smoker
        if (diabetes == 1) 0.874 else 0.0, // diabetes
        0.9533, // baselineSurvival
        86.61 // mean
      )
      case _ => (
        -29.799, // lnAge
        4.884, // lnAgeSquared
        13.540, // lnTotalCholesterol
        -3.114, // lnAge * lnTotalCholesterol
        -13.578, // lnHDLCholesterol
        3.149, // lnAge * lnHDLCholesterol
        2.019, // lnTreatedSBP
        0.0, // lnAge * lnTreatedSBP
        1.957, // lnUntreatedSBP
        0.0, // lnAge * lnUntreatedSBP
        if (smoker == 1) 7.574 else 0.0, // smoker
        -1.665, // lnAge * smoker
        if (diabetes == 1) 0.661 else 0.0, // diabetes
        0.9665, // baselineSurvival
        -29.18 // mean
      )
    }

    val lnSum = coefLnAge * lnAge +
      coefLnAgeSquared * pow(lnAge, 2) +
      coefLnTotalCholesterol * lnTotalCholesterol +
      coefLnAgeLnTotalCholesterol * lnAge * lnTotalCholesterol +
      coefLnHDLCholesterol * lnHDLCholesterol +
      coefLnAgeLnHDLCholesterol * lnAge * lnHDLCholesterol +
      (if (treatedHypertension == 1) coefLnTreatedSBP else coefLnUntreatedSBP) * lnSBP +
      (if (treatedHypertension == 1) coefLnAgeLnTreatedSBP else coefLnAgeLnUntreatedSBP) * lnAge * lnSBP +
      smokerCoefficient +
      coefLnAgeSmoker * lnAge * smoker +
      diabetesCoefficient

    100 * (1 - pow(baselineSurvival, exp(lnSum - mean)))
  }
}
