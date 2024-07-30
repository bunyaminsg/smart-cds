/**
 * This project utilizes the QRisk3 algorithm developed by Professor Julia Hippisley-Cox
 * and her team at the University of Nottingham. The algorithm is used to estimate the
 * 10-year risk of cardiovascular disease in patients, incorporating a range of risk factors.
 *
 * Citation:
 * Hippisley-Cox, J., Coupland, C., & Brindle, P. (2017). Development and validation of
 * QRISK3 risk prediction algorithms to estimate future risk of cardiovascular disease:
 * prospective cohort study. BMJ, 357, j2099. doi:10.1136/bmj.j2099. Available at
 * https://qrisk.org/.
 *
 * Acknowledgement:
 * We would like to thank the EMIS practices that contribute to QResearch, and EMIS and the
 * University of Nottingham for their expertise in establishing, developing, and supporting
 * the QResearch database. We also acknowledge the Office for National Statistics for
 * providing the mortality data, and NHS Digital for the Hospital Episode Statistics data.
 *
 * For more details on the QRisk3 algorithm, please refer to the full article published in BMJ:
 * Hippisley-Cox, J., Coupland, C., & Brindle, P. (2017). Development and validation of
 * QRISK3 risk prediction algorithms to estimate future risk of cardiovascular disease:
 * prospective cohort study. BMJ, 357, j2099. doi:10.1136/bmj.j2099. Available at
 * http://www.bmj.com/content/357/bmj.j2099.
 */

package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.util.{DateTimeUtil, SmokingCategoryEnum}
import scala.math.sqrt

object QRisk3FlowExecution {

  /**
   *
   * @param age Age of the patient
   * @param gender Gender of the patient
   * @param atrialFibrillation Atrial Fibrillation Condition Boolean
   * @param rheumatoidArthritis Rheumatoid Arthritis Condition Boolean
   * @param ckd345 Chronic Kidney Disease (Stage 3-4-5) Condition Boolean
   * @param type1Diabetes Type 1 Diabetes Condition Boolean
   * @param type2Diabetes Typ2 Diabetes Condition Boolean
   * @param hypertensiveTreatment Hypertensive Treatment Boolean
   * @param bmi Body Mass Index value of the patient
   * @param totalCholesterol Total Cholesterol of the patient
   * @param hdl HDL Cholesterol of the patient
   * @param bp Blood Pressure of the Patient
   * @param smokingStatus Current Smoking Status of the patient
   * @param cvdFmh Angina or heart attack in a 1st degree relative
   * @param cvd Cardiovascular disease Condition Boolean
   * @param atorvastatin Atorvastatin Medical Treatment Boolean
   * @param systemicLupusErythematosus Systemic Lupus Erythematosus Condition Boolean
   * @param severeMentalIllness Severe Mental Illness Condition Boolean
   * @param antipsychotics Antipsychotic Medical Treatment Boolean
   * @param erectileDysfunction Erectile Dysfunction Condition Boolean
   * @param migraine Migraine Condition Boolean
   * @param corticoSteroids Cortico Steroid Medical Treatment Boolean
   * @param ethnicity Ethnicity of the patient
   * @param responseBuilder Response Builder
   * @return
   */
  def executeFlow(age: Int, gender: String, atrialFibrillation: Int, rheumatoidArthritis: Int, ckd345: Int,
                  type1Diabetes: Int, type2Diabetes: Int, hypertensiveTreatment: Int, bmi: Option[Double],
                  totalCholesterol: Option[Double], hdl: Option[Double],
                  bp: Seq[Double], smokingStatus: Option[Int], cvdFmh: Int, cvd: Boolean, atorvastatin: Boolean, systemicLupusErythematosus: Int, severeMentalIllness: Int,
                  antipsychotics: Int, erectileDysfunction: Int, migraine: Int, corticoSteroids: Int, ethnicity: Option[Int],
                  responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {

    // Output that contains the QRisk3 Risk and Healthy Risk values
    var output = responseBuilder
    val riskScores = calculateQRisk(age, gender, atrialFibrillation, rheumatoidArthritis, ckd345, type1Diabetes, type2Diabetes,
      hypertensiveTreatment, bmi, totalCholesterol, hdl, bp, smokingStatus, cvdFmh, systemicLupusErythematosus, severeMentalIllness, antipsychotics,
      erectileDysfunction, migraine, corticoSteroids, ethnicity)

    if (riskScores.isDefined) {
      output = output.withCard(_.loadCardWithPostTranslation("card-score",
        "effectiveDate" -> DateTimeUtil.zonedNow(), // The date of measurement
        "scoreValue" -> riskScores.get._1, // QRisk3 Risk Score
        "healthyValue" -> riskScores.get._2 // QRisk3 Risk Score of a healthy person
      ))

      /**
       * Adding suggestions for patient if needed
       */
      output = recommendAtorvastatinIfApplicable(riskScores.get._1, type2Diabetes == 1, ckd345 == 1, cvd, atorvastatin, hdl, totalCholesterol, output)
      output = recommendStopSmokingIfApplicable(smokingStatus, output)
      output = recommendReduceBPIfApplicable(bp, output)
      output = recommendReduceBMIIfApplicable(bmi, output)
    }
    output
  }

  /**
   *
   * @param bmi Body Mass Index value of the patient
   * @param output output
   * @return If the BMI value of the patient is bigger than 25, patient is suggested to lower his/her BMI value
   */
  private def recommendReduceBMIIfApplicable(bmi: Option[Double], output: CdsResponseBuilder) = {
    if (bmi.exists(_ > 25)) {
      output.withCard(_.loadCardWithPostTranslation("card-reduce-bmi",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  /**
   *
   * @param sbp Systolic Blood Pressure value of the patient
   * @param output output
   * @return If the SBP value of the patient is bigger than 140, patient is suggested to lower is SBP value
   */
  private def recommendReduceBPIfApplicable(sbp: Seq[Double], output: CdsResponseBuilder) = {
    if (sbp.exists(y => {y > 140})) {
      output.withCard(_.loadCardWithPostTranslation("card-reduce-bp",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  /**
   *
   * @param smokingStatus An integer that represents that smoking habbit of the patient
   * @param output output
   * @return If the patient is heavily addicted to smoking, he/she is suggested to quit smoking
   */
  private def recommendStopSmokingIfApplicable(smokingStatus: Option[Int], output: CdsResponseBuilder): CdsResponseBuilder = {
    if (smokingStatus.get >= SmokingCategoryEnum.LIGHT) {
      output.withCard(_.loadCardWithPostTranslation("card-stop-smoking",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  /**
   *
   * @param riskScore The QRisk3 risk score value of the patient
   * @param t2d Type 2 Diabetes Flag
   * @param ckd Chronic Kidney Disease Flag
   * @param cvd CVD Flag
   * @param atorvastatin Atorvastatin Flag (True if the patient is on it)
   * @param hdl HDL Value of the patient
   * @param cholesterol Total Cholesterol of the patient
   * @param output output
   * @return If a patient has Type2Diabetes without CKD and taking Atorvastatin and
   *         if that patient has CVD or has a risk score aboce 10%,
   *         that user suggested to use Atorvastatin to reduce non-hdl cholesterol
   */
  private def recommendAtorvastatinIfApplicable(riskScore: Double, t2d: Boolean, ckd: Boolean, cvd: Boolean, atorvastatin: Boolean,
                                                hdl: Option[Double], cholesterol: Option[Double], output: CdsResponseBuilder): CdsResponseBuilder = {
    if(cholesterol.isDefined && hdl.isDefined) {
      val _cholesterol = cholesterol.get
      val _hdl = hdl.get
      val targetCholesterol = _cholesterol - ((_cholesterol - _hdl) * 0.4)
      if (t2d && !ckd && !atorvastatin) {
        if (cvd) {
          output.withCard(_.loadCardWithPostTranslation("card-reduce-non-hdl",
            "effectiveDate" -> DateTimeUtil.zonedNow(),
            "dose" -> 80,
            "value" -> targetCholesterol
          ))
        } else if (riskScore > 10) {
          output.withCard(_.loadCardWithPostTranslation("card-reduce-non-hdl",
            "effectiveDate" -> DateTimeUtil.zonedNow(),
            "dose" -> 20,
            "value" -> targetCholesterol
          ))
        } else {
          output
        }
      } else {
        output
      }
    } else {
      output
    }
  }

  /**
   *
   * @param sbpList Systolic Blood Pressure Measurement list of the patient (5 max)
   * @return If at least last 2 SBP measurement of the patient is known,
   *         this function returns that Standart deviation of that measurements
   */
  private def sbpStdDevCalc(sbpList:Seq[Double]): Double = {
    if(sbpList.length >= 2) {
      val sbpMean: Double = sbpList.sum / sbpList.length
      var output: Double = 0
      for(n <- sbpList){
        output = output + ((sbpMean-n)*(sbpMean-n))
      }
      output = output/sbpList.length
      sqrt(output)
    }
    else{
      0
    }
  }

  /**
   * Validates given prefetch and returns the QRISK3 score
   */
  def calculateQRisk(age: Int, gender: String, b_AF: Int, b_ra: Int, b_renal: Int, b_type1: Int, b_type2: Int, b_treatedhyp: Int,
                     bmiOpt: Option[Double], cholesterolOpt: Option[Double], hdlOpt: Option[Double], sbpOpt: Seq[Double], smokingCategory: Option[Int],
                     fh_cvd: Int, systemicLupusErythematosus: Int, severeMentalIllness: Int,
                     antipsychotics: Int, erectileDysfunction: Int, migraine: Int, corticoSteroids: Int,
                     ethnicity: Option[Int]): Option[(Double, Double)] = {

    /* Checking if smokingCategory and Ethnicity of the patient is supplied */
    if (smokingCategory.isEmpty || ethnicity.isEmpty || bmiOpt.isEmpty || cholesterolOpt.isEmpty || sbpOpt.isEmpty || hdlOpt.isEmpty) {
      println("Required inputs are not found")
      return None
    }

    /* Setting up the inputs of the algorithm */
    val validAge = if(age < 25) {25} else if(age > 85) {85} else age
    val bmi = bmiOpt.get
    val cholesterol = cholesterolOpt.get
    val hdl = hdlOpt.get
    val sbp = sbpOpt.head
    val sbpStd: Double = sbpStdDevCalc(sbpOpt)
    val rati: Double = cholesterol / hdl
    val smoke_cat = smokingCategory.get

    val surv = 10
    val town = 0
    val ethrisk = ethnicity.get

    /* A print statement in order to trace the inputs */
    println(s"Calculating QRisk3 Risk with values: age=$age,bmi=$bmi totalCholesterol=$cholesterol, hdlCholesterol=$hdl, sbp=$sbp, type1=$b_type1, treatedHypertension=$b_treatedhyp, race=$ethrisk, smoke_cat=$smoke_cat, sbp5=$sbpStd, fh_cvd=$fh_cvd, b_treatedhyp=$b_treatedhyp, atypical=$antipsychotics")

    if (gender == "male") {
      val patientScore = cvdMaleRaw(validAge, b_AF, antipsychotics, corticoSteroids, erectileDysfunction, migraine, b_ra, b_renal, severeMentalIllness, systemicLupusErythematosus, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpStd, smoke_cat, surv, town)
      val healthyScore = cvdMaleRaw(validAge, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 3.33, 125, 0, 0, surv, town)
      Some(patientScore,healthyScore)
    } else {
      val patientScore = cvdFemaleRaw(validAge, b_AF, antipsychotics, corticoSteroids, migraine, b_ra, b_renal, severeMentalIllness, systemicLupusErythematosus, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpStd, smoke_cat, surv, town)
      val healthyScore = cvdFemaleRaw(validAge, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 3.33, 125, 0, 0, surv, town)
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
