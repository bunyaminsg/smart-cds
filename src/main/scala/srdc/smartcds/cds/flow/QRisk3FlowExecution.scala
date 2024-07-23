package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir.{Condition, FamilyMemberHistory, MedicationStatement, Observation, Patient}
import srdc.smartcds.util.{DateTimeUtil, FhirParseHelper, UnitConceptEnum}

import scala.math.{exp, pow, sqrt}
import scala.util.Try

object QRisk3FlowExecution {

  /**
   * Executes QRISK3 calculation service
   * @param patient Patient resource
   * @param AtrialFibrillation Atrial Fibrillation Condition
   * @param RheumatoidArthritis Rheumatoid Arthritis Condition
   * @param CKD3_4_5 Chronic kidney disease (stage 3, 4 or 5)
   * @param Type1Diabetes Type 1 Diabetes Condition
   * @param Type2Diabetes Type 1 Diabetes Condition
   * @param HypertensiveTreatment Hypertensive Treatment
   * @param BMI Body Mass Index Observation
   * @param TotalCholesterol Total Cholesterol Observation
   * @param HDL HDL Observation
   * @param BP_SBP Systolic Blood Pressure Observation
   * @param SmokingStatus Smoking Status Observation
   * @param CVD_FMH Angina or heart attack in a 1st degree relative
   * @param Corticosteroids Cortico Steroid Medical Treatment
   * @param Antipsychotics Antipsychotic Medical Treatment
   * @param Ethnicity Ethnicity Observation
   * @param SystemicLupusErythematosus Systemic Lupus Erythematosus Condition
   * @param SevereMentalIllness Severe Mental Illness Condition
   * @param ErectileDysfunction Erectile Dysfunction Condition
   * @param Migraine Migraine Condition
   * @param responseBuilder Response Builder
   * @return
   */
  def executeFlow(patient: Patient, AtrialFibrillation: Seq[Condition], RheumatoidArthritis: Seq[Condition], CKD3_4_5: Seq[Condition],
                  Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition], HypertensiveTreatment: Seq[MedicationStatement],
                  BMI: Seq[Observation], TotalCholesterol: Seq[Observation], HDL: Seq[Observation], BP_SBP: Seq[Observation], SmokingStatus: Seq[Observation],
                  CVD_FMH: Seq[FamilyMemberHistory], Corticosteroids: Seq[MedicationStatement], Antipsychotics: Seq[MedicationStatement],
                  Ethnicity: Seq[Observation], SystemicLupusErythematosus: Seq[Condition], SevereMentalIllness: Seq[Condition],
                  ErectileDysfunction: Seq[Condition], Migraine: Seq[Condition], responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {

    var output = responseBuilder
    val riskScores = calculateQRisk(patient, AtrialFibrillation, RheumatoidArthritis, CKD3_4_5, Type1Diabetes, Type2Diabetes,
      HypertensiveTreatment, BMI, TotalCholesterol, HDL, BP_SBP, SmokingStatus, CVD_FMH, Corticosteroids, Antipsychotics,
      Ethnicity, SystemicLupusErythematosus, SevereMentalIllness, ErectileDysfunction, Migraine)

    if (riskScores.isDefined) {
      output = output.withCard(_.loadCardWithPostTranslation("card-score",
        "effectiveDate" -> DateTimeUtil.zonedNow(),
        "scoreValue" -> riskScores.get._1,
        "healthyValue" -> riskScores.get._2
      ))
    }

    output
  }

  /**
   * Validates given prefetch and returns the QRISK3 score
   */
  def calculateQRisk(patient: Patient, AtrialFibrillation: Seq[Condition], RheumatoidArthritis: Seq[Condition], CKD3_4_5: Seq[Condition],
                     Type1Diabetes: Seq[Condition], Type2Diabetes: Seq[Condition], HypertensiveTreatment: Seq[MedicationStatement],
                     BMI: Seq[Observation], TotalCholesterol: Seq[Observation], HDL: Seq[Observation], BP_SBP: Seq[Observation], SmokingStatus: Seq[Observation],
                     CVD_FMH: Seq[FamilyMemberHistory], Corticosteroids: Seq[MedicationStatement], Antipsychotics: Seq[MedicationStatement],
                     Ethnicity: Seq[Observation], SystemicLupusErythematosus: Seq[Condition], SevereMentalIllness: Seq[Condition],
                     ErectileDysfunction: Seq[Condition], Migraine: Seq[Condition]): Option[(Double, Double)] = {
    val checkExists = (resources: Seq[Any]) => if (resources.nonEmpty) 1 else 0

    val age = FhirParseHelper.getAge(patient)
    val b_AF = checkExists(AtrialFibrillation)
    val b_atypicalantipsy = checkExists(Antipsychotics)
    val b_corticosteroids = checkExists(Corticosteroids)
    val b_impotence2 = checkExists(ErectileDysfunction)
    val b_migraine = checkExists(Migraine)
    val b_ra = checkExists(RheumatoidArthritis)
    val b_renal = checkExists(CKD3_4_5)
    val b_semi = checkExists(SevereMentalIllness)
    val b_sle = checkExists(SystemicLupusErythematosus)
    val b_treatedhyp = checkExists(HypertensiveTreatment)
    val b_type1 = checkExists(Type1Diabetes)
    val b_type2 = checkExists(Type2Diabetes)
    val bmiOpt = Try(BMI.head.valueQuantity.get.value.get).toOption
    var bmi: Double = 0
    val fh_cvd = checkExists(CVD_FMH)

    val cholesterolObs = TotalCholesterol.headOption
    val hdlObs = HDL.headOption

    val sbpOpt = FhirParseHelper.getSystolicBP(BP_SBP)

    def sbpStdDeviation(observations: Seq[Observation]): Double = {
      if(observations.length >= 2){
        var output:Double = 0
        var sum:Double = 0
        var l = Seq[Double]()
        for (n <- observations){
          val seq: Seq[Observation] = Seq(n)
          val sbpOptDeviation = FhirParseHelper.getSystolicBP(seq)
          if(sbpOptDeviation.isDefined){
            l = l :+ sbpOptDeviation.get
            sum = sum + sbpOptDeviation.get
          }
        }
        if(l.length >= 2){
          sum = sum/l.length
          for(n <- l){
            output = output + ((sum-n)*(sum-n))
          }
          output = output/l.length
          sqrt(output)
        }
        else{
          0
        }
      }
      else{
        0
      }
    }

    val smokingObs = SmokingStatus.headOption

    var rati: Double = 4

    var cholesterol: Double = 0
    var hdl: Double = 0

    if (FhirParseHelper.checkObservationValuesExist(List(cholesterolObs, hdlObs))) {
      cholesterol = FhirParseHelper.getQuantityObservationValue(cholesterolObs, Option(UnitConceptEnum.CHOLESTEROL)).get
      hdl = FhirParseHelper.getQuantityObservationValue(hdlObs, Option(UnitConceptEnum.CHOLESTEROL)).get
      rati = cholesterol / hdl
    }

    val sbp = if(sbpOpt.isEmpty) {125} else {sbpOpt.get}
    val sbpDeviation = sbpStdDeviation(BP_SBP)

    val smoking = if (smokingObs.isDefined && smokingObs.get.valueCodeableConcept.isDefined) {
      smokingObs.get.valueCodeableConcept.get.coding.map(_.code).toSeq
    } else {Seq("266919005")}
    val smoke_cat = if (Seq("LA18978-9", "LA18980-5", "266919005").intersect(smoking).nonEmpty) 0
    else if (smoking.contains("LA15920-4", "8517006")) 1
    else if (Seq("LA18977-1", "LA18982-1").intersect(smoking).nonEmpty) 2
    else if (Seq("LA18979-7", "LA18976-3", "449868002").intersect(smoking).nonEmpty) 3
    else if (smoking.contains("LA18981-3")) 4
    else 0

    val maleIndicator: Boolean = patient.gender.contains("male")
    val surv = 10
    val town = 0
    val ethriskCode = Ethnicity.headOption.map(_.valueCodeableConcept.flatMap(_.coding.headOption.map(_.code)).getOrElse("0"))
    val ethrisk: Int = ethriskCode match {
      case Some("0") =>
        if(maleIndicator){bmi = 27.1}
        else{bmi = 27.5}
        1
      case Some("1") =>
        if(maleIndicator){bmi = 25.3}
        else{bmi = 26.6}
        2
      case Some("2") =>
        if(maleIndicator){bmi = 26.2}
        else{bmi = 28.3}
        3
      case Some("3") =>
        if(maleIndicator){bmi = 24.9}
        else{bmi = 27}
        4
      case Some("4") =>
        if(maleIndicator){bmi = 25.4}
        else{bmi = 25.8}
        5
      case Some("5") =>
        if(maleIndicator){bmi = 26.7}
        else{bmi = 29.1}
        6
      case Some("6") =>
        if(maleIndicator){bmi = 26.3  }
        else{bmi = 29.5}
        7
      case Some("7") =>
        if(maleIndicator){bmi = 23.8}
        else{bmi = 23.9}
        8
      case Some("8") =>
        if(maleIndicator){bmi = 26.3}
        else{bmi = 27.5}
        9
      case _ =>
        if(maleIndicator){bmi = 27.1}
        else{bmi = 27.5}
        0
    }

    if(bmiOpt.isDefined){bmi = bmiOpt.get}

    println(s"Calculating QRisk3 Risk with values: age=$age,bmi=$bmi totalCholesterol=$cholesterol, hdlCholesterol=$hdl, sbp=$sbp, type1=$b_type1, treatedHypertension=$b_treatedhyp, race=$ethrisk, smoke_cat=$smoke_cat, sbp5=$sbpDeviation, fh_cvd=$fh_cvd, b_treatedhyp=$b_treatedhyp, atypical=$b_atypicalantipsy")

    if (maleIndicator) {
      val patientScore = cvdMaleRaw(age, b_AF, b_atypicalantipsy, b_corticosteroids, b_impotence2, b_migraine, b_ra, b_renal, b_semi, b_sle, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpDeviation, smoke_cat, surv, town)
      val healthyScore = cvdMaleRaw(age, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 4, 125, 0, 0, surv, town)
      Some(patientScore,healthyScore)
    } else {
      val patientScore = cvdFemaleRaw(age, b_AF, b_atypicalantipsy, b_corticosteroids, b_migraine, b_ra, b_renal, b_semi, b_sle, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpDeviation, smoke_cat, surv, town)
      val healthyScore = cvdFemaleRaw(age, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 4, 125, 0, 0, surv, town)
      Some(patientScore,healthyScore)
    }
  }

  /**
   * Calculate QRISK3 score for male patients
   */
  def cvdMaleRaw(
                  age: Int, b_AF: Int, b_atypicalantipsy: Int, b_corticosteroids: Int, b_impotence2: Int, b_migraine: Int, b_ra: Int, b_renal: Int, b_semi: Int, b_sle: Int, b_treatedhyp: Int, b_type1: Int, b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, sbps5: Double, smoke_cat: Int, surv: Int, town: Double
                ): Double = {

    val survivor: Array[Double] = Array(
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0.977268040180206,
      0,
      0,
      0,
      0,
      0
    )

    /* The conditional arrays */

    val Iethrisk: Array[Double] = Array(
      0,
      0,
      0.27719248760308279,
      0.47446360714931268,
      0.52961729919689371,
      0.035100159186299017,
      -0.35807899669327919,
      -0.4005648523216514,
      -0.41522792889830173,
      -0.26321348134749967
    )

    val Ismoke: Array[Double] = Array(
      0,
      0.19128222863388983,
      0.55241588192645552,
      0.63835053027506072,
      0.78983819881858019
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    var dage: Double = age.toDouble
    dage = dage / 10
    var age_1 = math.pow(dage, -1)
    var age_2 = math.pow(dage, 3)
    var dbmi: Double = bmi
    dbmi = dbmi / 10
    var bmi_2 = math.pow(dbmi, -2) * math.log(dbmi)
    var bmi_1 = math.pow(dbmi, -2)

    /* Centring the continuous variables */

    age_1 = age_1 - 0.234766781330109
    age_2 = age_2 - 77.284080505371094
    bmi_1 = bmi_1 - 0.149176135659218
    bmi_2 = bmi_2 - 0.141913309693336
    val _rati = rati - 4.300998687744141
    val _sbp = sbp - 128.57157897949219
    val _sbps5 = sbps5 - 8.756621360778809
    val _town = town - 0.52630490064621

    /* Start of Sum */
    var a: Double = 0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * -17.839781666005575
    a += age_2 * 0.0022964880605765492
    a += bmi_1 * 2.4562776660536358
    a += bmi_2 * -8.301112231471135
    a += _rati * 0.17340196856327111
    a += _sbp * 0.012910126542553305
    a += _sbps5 * 0.010251914291290456
    a += _town * 0.033268201277287295

    /* Sum from boolean values */

    a += b_AF * 0.88209236928054657
    a += b_atypicalantipsy * 0.13046879855173513
    a += b_corticosteroids * 0.45485399750445543
    a += b_impotence2 * 0.22251859086705383
    a += b_migraine * 0.25584178074159913
    a += b_ra * 0.20970658013956567
    a += b_renal * 0.71853261288274384
    a += b_semi * 0.12133039882047164
    a += b_sle * 0.4401572174457522
    a += b_treatedhyp * 0.51659871082695474
    a += b_type1 * 1.2343425521675175
    a += b_type2 * 0.85942071430932221
    a += fh_cvd * 0.54055469009390156

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat == 1) -0.21011133933516346 else 0)
    a += age_1 * (if (smoke_cat == 2) 0.75268676447503191 else 0)
    a += age_1 * (if (smoke_cat == 3) 0.99315887556405791 else 0)
    a += age_1 * (if (smoke_cat == 4) 2.1331163414389076 else 0)
    a += age_1 * b_AF * 3.4896675530623207
    a += age_1 * b_corticosteroids * 1.1708133653489108
    a += age_1 * b_impotence2 * -1.506400985745431
    a += age_1 * b_migraine * 2.3491159871402441
    a += age_1 * b_renal * -0.50656716327223694
    a += age_1 * b_treatedhyp * 6.5114581098532671
    a += age_1 * b_type1 * 5.3379864878006531
    a += age_1 * b_type2 * 3.6461817406221311
    a += age_1 * bmi_1 * 31.004952956033886
    a += age_1 * bmi_2 * -111.29157184391643
    a += age_1 * fh_cvd * 2.7808628508531887
    a += age_1 * _sbp * 0.018858524469865853
    a += age_1 * _town * -0.1007554870063731
    a += age_2 * (if (smoke_cat == 1) -0.00049854870275326121 else 0)
    a += age_2 * (if (smoke_cat == 2) -0.00079875633317385414 else 0)
    a += age_2 * (if (smoke_cat == 3) -0.00083706184266251296 else 0)
    a += age_2 * (if (smoke_cat == 4) -0.00078400319155637289 else 0)
    a += age_2 * b_AF * -0.00034995608340636049
    a += age_2 * b_corticosteroids * -0.0002496045095297166
    a += age_2 * b_impotence2 * -0.0011058218441227373
    a += age_2 * b_migraine * 0.00019896446041478631
    a += age_2 * b_renal * -0.0018325930166498813
    a += age_2 * b_treatedhyp * 0.00063838053104165013
    a += age_2 * b_type1 * 0.0006409780808752897
    a += age_2 * b_type2 * -0.00024695695588868315
    a += age_2 * bmi_1 * 0.0050380102356322029
    a += age_2 * bmi_2 * -0.013074483002524319
    a += age_2 * fh_cvd * -0.00024791809907396037
    a += age_2 * _sbp * -0.00001271874191588457
    a += age_2 * _town * -0.000093299642323272888

    /* Calculate the score itself */
    val score = 100.0 * (1 - math.pow(survivor(surv), math.exp(a)))
    score
  }

  /**
   * Calculate QRISK3 score for female patients
   */
  def cvdFemaleRaw(
                    age: Int, b_AF: Int, b_atypicalantipsy: Int, b_corticosteroids: Int, b_migraine: Int, b_ra: Int, b_renal: Int, b_semi: Int, b_sle: Int, b_treatedhyp: Int, b_type1: Int, b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, sbps5: Double, smoke_cat: Int, surv: Int, town: Double
                  ): Double = {

    val survivor: Array[Double] = Array(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.988876402378082, 0, 0, 0, 0, 0
    )

    /* The conditional arrays */

    val Iethrisk: Array[Double] = Array(
      0, 0, 0.28040314332995425, 0.56298994142075398, 0.29590000851116516, 0.072785379877982545,
      -0.17072135508857317, -0.39371043314874971, -0.32632495283530272, -0.17127056883241784
    )

    val Ismoke: Array[Double] = Array(
      0, 0.13386833786546262, 0.56200858012438537, 0.66749593377502547, 0.84948177644830847
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    var dage: Double = age.toDouble
    dage = dage / 10
    var age_1 = math.pow(dage, -2)
    var age_2 = dage
    var dbmi: Double = bmi
    dbmi = dbmi / 10
    var bmi_1 = math.pow(dbmi, -2)
    var bmi_2 = math.pow(dbmi, -2) * math.log(dbmi)

    /* Centring the continuous variables */

    age_1 = age_1 - 0.053274843841791
    age_2 = age_2 - 4.332503318786621
    bmi_1 = bmi_1 - 0.154946178197861
    bmi_2 = bmi_2 - 0.144462317228317
    val _rati = rati - 3.47632646560669
    val _sbp = sbp - 123.13001251220703
    val _sbps5 = sbps5 - 9.002537727355957
    val _town = town - 0.392308831214905

    /* Start of Sum */
    var a: Double = 0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * -8.138810924772619
    a += age_2 * 0.797333766896991
    a += bmi_1 * 0.2923609227546005
    a += bmi_2 * -4.151330021383767
    a += _rati * 0.15338035820802554
    a += _sbp * 0.013131488407103424
    a += _sbps5 * 0.0078894541014586095
    a += _town * 0.07722379058859011

    /* Sum from boolean values */

    a += b_AF * 1.5923354969269663
    a += b_atypicalantipsy * 0.25237642070115557
    a += b_corticosteroids * 0.5952072530460185
    a += b_migraine * 0.301267260870345
    a += b_ra * 0.21364803435181942
    a += b_renal * 0.6519456949384583
    a += b_semi * 0.12555308058820178
    a += b_sle * 0.7588093865426769
    a += b_treatedhyp * 0.50931593683423004
    a += b_type1 * 1.7267977510537347
    a += b_type2 * 1.0688773244615468
    a += fh_cvd * 0.45445319020896213

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat == 1) -4.705716178585189 else 0)
    a += age_1 * (if (smoke_cat == 2) -2.7430383403573337 else 0)
    a += age_1 * (if (smoke_cat == 3) -0.8660808882939218 else 0)
    a += age_1 * (if (smoke_cat == 4) 0.9024156236971065 else 0)
    a += age_1 * b_AF * 19.93803488954656
    a += age_1 * b_corticosteroids * -0.9840804523593628
    a += age_1 * b_migraine * 1.7634979587872999
    a += age_1 * b_renal * -3.5874047731694114
    a += age_1 * b_sle * 19.690303738638292
    a += age_1 * b_treatedhyp * 11.872809733921812
    a += age_1 * b_type1 * -1.2444332714320747
    a += age_1 * b_type2 * 6.86523420000096
    a += age_1 * bmi_1 * 23.802623412141742
    a += age_1 * bmi_2 * -71.18494769208701
    a += age_1 * fh_cvd * 0.9946780794043513
    a += age_1 * _sbp * 0.034131842338615485
    a += age_1 * _town * -1.0301180802035639
    a += age_2 * (if (smoke_cat == 1) -0.07558924464319303 else 0)
    a += age_2 * (if (smoke_cat == 2) -0.11951192874867074 else 0)
    a += age_2 * (if (smoke_cat == 3) -0.10366306397571923 else 0)
    a += age_2 * (if (smoke_cat == 4) -0.13991853591718389 else 0)
    a += age_2 * b_AF * -0.0761826510111625
    a += age_2 * b_corticosteroids * -0.12005364946742472
    a += age_2 * b_migraine * -0.06558691789869986
    a += age_2 * b_renal * -0.22688873086442507
    a += age_2 * b_sle * 0.07734794967901627
    a += age_2 * b_treatedhyp * 0.0009685782358817444
    a += age_2 * b_type1 * -0.2872406462448895
    a += age_2 * b_type2 * -0.09711225259069549
    a += age_2 * bmi_1 * 0.5236995893366443
    a += age_2 * bmi_2 * 0.04574419012232376
    a += age_2 * fh_cvd * -0.07688505169842304
    a += age_2 * _sbp * -0.0015082501423272358
    a += age_2 * _town * -0.03159341467496233

    /* Calculate the score itself */
    val score = 100.0 * (1 - math.pow(survivor(surv), math.exp(a)))
    score
  }
}
