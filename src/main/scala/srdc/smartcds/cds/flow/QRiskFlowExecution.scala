package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.util.{DateTimeUtil, SmokingCategoryEnum}

import scala.math.{exp, pow}

object QRiskFlowExecution {

  /**
   * Executes QRISK calculation service
   * @param age
   * @param gender
   * @param atrialFibrillation
   * @param rheumatoidArthritis
   * @param ckd45
   * @param type1Diabetes
   * @param type2Diabetes
   * @param hypertensiveTreatment
   * @param bmi
   * @param totalCholesterol
   * @param hdl
   * @param bp
   * @param smokingStatus
   * @param cvdFmh
   * @param cvd
   * @param atorvastatin
   * @param responseBuilder
   * @return
   */
  def executeFlow(age: Int, gender: String, atrialFibrillation: Int, rheumatoidArthritis: Int, ckd45: Int,
                          type1Diabetes: Int, type2Diabetes: Int, hypertensiveTreatment: Int, bmi: Option[Double],
                          totalCholesterol: Option[Double], hdl: Option[Double],
                          bp: Option[Double], smokingStatus: Option[Int], cvdFmh: Int, cvd: Boolean, atorvastatin: Boolean,
                          responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {
    var output = responseBuilder
    val riskScores = calculateQRisk(age, gender, atrialFibrillation, rheumatoidArthritis, ckd45, type1Diabetes, type2Diabetes,
      hypertensiveTreatment, bmi, totalCholesterol, hdl, bp, smokingStatus, cvdFmh)

    if (riskScores.isDefined) {
      output = output.withCard(_.loadCardWithPostTranslation("card-score",
        "effectiveDate" -> DateTimeUtil.zonedNow(),
        "scoreValue" -> riskScores.get._1,
        "healthyValue" -> riskScores.get._2
      ))
      output = recommendAtorvastatinIfApplicable(riskScores.get._1, type2Diabetes == 1, ckd45 == 1, cvd, atorvastatin, hdl, totalCholesterol, output)
      output = recommendStopSmokingIfApplicable(smokingStatus, output)
      output = recommendReduceBPIfApplicable(bp, output)
      output = recommendReduceBMIIfApplicable(bmi, output)
    }

    output
  }

  private def recommendReduceBMIIfApplicable(bmi: Option[Double], output: CdsResponseBuilder) = {
    if (bmi.exists(_ > 25)) {
      output.withCard(_.loadCardWithPostTranslation("card-reduce-bmi",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  private def recommendReduceBPIfApplicable(sbp: Option[Double], output: CdsResponseBuilder) = {
    if (sbp.exists(_ > 140)) {
      output.withCard(_.loadCardWithPostTranslation("card-reduce-bp",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  private def recommendStopSmokingIfApplicable(smokingStatus: Option[Int], output: CdsResponseBuilder): CdsResponseBuilder = {
    if (smokingStatus.get >= SmokingCategoryEnum.LIGHT) {
      output.withCard(_.loadCardWithPostTranslation("card-stop-smoking",
        "effectiveDate" -> DateTimeUtil.zonedNow()
      ))
    } else {
      output
    }
  }

  private def recommendAtorvastatinIfApplicable(riskScore: Double, t2d: Boolean, ckd: Boolean, cvd: Boolean, atorvastatin: Boolean,
                                        hdl: Option[Double], cholesterol: Option[Double], output: CdsResponseBuilder): CdsResponseBuilder = {
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
  }

  /**
   * Validates given prefetch and returns the QRISK score
   */
  def calculateQRisk(age: Int, gender: String, b_AF: Int, b_ra: Int, b_renal: Int, b_type1: Int, b_type2: Int, b_treatedhyp: Int,
                     bmiOpt: Option[Double], cholesterolOpt: Option[Double], hdlOpt: Option[Double], sbpOpt: Option[Double], smokingCategory: Option[Int],
                     fh_cvd: Int): Option[(Double, Double)] = {

    if (bmiOpt.isEmpty || sbpOpt.isEmpty || hdlOpt.isEmpty || cholesterolOpt.isEmpty || smokingCategory.isEmpty) {
      println("BMI, SBP, Smoking, Cholesterol or HDL not found")
      return None
    }

    val bmi = bmiOpt.get
    val cholesterol = cholesterolOpt.get
    val hdl = hdlOpt.get
    val sbp = sbpOpt.get

    val rati: Double = cholesterol / hdl

    val smoke_cat = smokingCategory.get

    val surv = 10
    val town = 0
    val ethrisk = 0

    if (gender == "male") {
      val patientScore = calculateQRiskM(age, b_AF, b_ra, b_renal, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, smoke_cat, surv, town)
      val healthyScore = calculateQRiskM(age, 0, 0, 0, 0, 0, 0, 25, 0, 0, 4, 125, 0, surv, town)
      Some(patientScore,healthyScore)
    } else {
      val patientScore = calculateQRiskF(age, b_AF, b_ra, b_renal, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, smoke_cat, surv, town)
      val healthyScore = calculateQRiskF(age, 0, 0, 0, 0, 0, 0, 25, 0, 0, 4, 125, 0, surv, town)
      Some(patientScore,healthyScore)
    }
  }

  /**
   * Calculate QRISK score for male patients
   */
  def calculateQRiskM(age: Int, b_AF: Int, b_ra: Int, b_renal: Int, b_treatedhyp: Int, b_type1: Int,
                      b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, smoke_cat: Int,
                      surv: Int, town: Double): Double = {

    val survivor: Seq[Double] = Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.978794217109680, 0, 0, 0, 0, 0)

    /* The conditional arrays */

    val Iethrisk: Seq[Double] = Seq(
      0,
      0,
      0.3173321430481919100000000,
      0.4738590786081115500000000,
      0.5171314655968145500000000,
      0.1370301157366419200000000,
      -0.3885522304972663900000000,
      -0.3812495485312194500000000,
      -0.4064461381650994500000000,
      -0.2285715521377336100000000
    )

    val Ismoke: Seq[Double] = Seq(
      0,
      0.2684479158158020200000000,
      0.6307674973877591700000000,
      0.7178078883378695700000000,
      0.8704172533465485100000000
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    var dage: Double = age
    dage = dage / 10
    var age_1 = pow(dage,-1)
    var age_2 = pow(dage,2)
    var dbmi: Double = bmi
    dbmi = dbmi / 10
    var bmi_2 = pow(dbmi,-2)*scala.math.log(dbmi)
    var bmi_1 = pow(dbmi,-2)

    /* Centring the continuous variables */

    age_1 = age_1 - 0.233734160661697
    age_2 = age_2 - 18.304403305053711
    bmi_1 = bmi_1 - 0.146269768476486
    bmi_2 = bmi_2 - 0.140587374567986
    val _rati = rati - 4.321151256561279
    val _sbp = sbp - 130.589752197265620
    val _town = town - 0.551009356975555

    /* Start of Sum */
    var a: Double = 0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * -18.0437312550377270000000000
    a += age_2 * 0.0236486454254306940000000
    a += bmi_1 * 2.5388084343581578000000000
    a += bmi_2 * -9.1034725871528597000000000
    a += _rati * 0.1684397636136909500000000
    a += _sbp * 0.0105003089380754820000000
    a += _town * 0.0323801637634487590000000

    /* Sum from boolean values */

    a += b_AF * 1.0363048000259454000000000
    a += b_ra * 0.2519953134791012600000000
    a += b_renal * 0.8359352886995286000000000
    a += b_treatedhyp * 0.6603459695917862600000000
    a += b_type1 * 1.3309170433446138000000000
    a += b_type2 * 0.9454348892774417900000000
    a += fh_cvd * 0.5986037897136281500000000

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat==1) 0.6186864699379683900000000 else 0)
    a += age_1 * (if (smoke_cat==2) 1.5522017055600055000000000 else 0)
    a += age_1 * (if (smoke_cat==3) 2.4407210657517648000000000 else 0)
    a += age_1 * (if (smoke_cat==4) 3.5140494491884624000000000 else 0)
    a += age_1 * b_AF * 8.0382925558108482000000000
    a += age_1 * b_renal * -1.6389521229064483000000000
    a += age_1 * b_treatedhyp * 8.4621771382346651000000000
    a += age_1 * b_type1 * 5.4977016563835504000000000
    a += age_1 * b_type2 * 3.3974747488766690000000000
    a += age_1 * bmi_1 * 33.8489881012767600000000000
    a += age_1 * bmi_2 * -140.6707025404897100000000000
    a += age_1 * fh_cvd * 2.0858333154353321000000000
    a += age_1 * _sbp * 0.0501283668830720540000000
    a += age_1 * _town * -0.1988268217186850700000000
    a += age_2 * (if (smoke_cat==1) -0.0040893975066796338000000 else 0)
    a += age_2 * (if (smoke_cat==2) -0.0056065852346001768000000 else 0)
    a += age_2 * (if (smoke_cat==3) -0.0018261006189440492000000 else 0)
    a += age_2 * (if (smoke_cat==4) -0.0014997157296173290000000 else 0)
    a += age_2 * b_AF * 0.0052471594895864343000000
    a += age_2 * b_renal * -0.0179663586193546390000000
    a += age_2 * b_treatedhyp * 0.0092088445323379176000000
    a += age_2 * b_type1 * 0.0047493510223424558000000
    a += age_2 * b_type2 * -0.0048113775783491563000000
    a += age_2 * bmi_1 * 0.0627410757513945650000000
    a += age_2 * bmi_2 * -0.2382914909385732100000000
    a += age_2 * fh_cvd * -0.0049971149213281010000000
    a += age_2 * _sbp * -0.0000523700987951435090000
    a += age_2 * _town * -0.0012518116569283104000000

    /* Calculate the score itself */
    100.0 * (1 - pow(survivor(surv), exp(a)))
  }

  /**
   * Calculate QRISK score for female patients
   */
  def calculateQRiskF(age: Int, b_AF: Int, b_ra: Int, b_renal: Int, b_treatedhyp: Int, b_type1: Int,
                      b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, smoke_cat: Int,
                      surv: Int, town: Double): Double = {

    val survivor = Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.989747583866119, 0, 0, 0, 0, 0)

    /* The conditional arrays */

    val Iethrisk = Seq(
      0,
      0,
      0.2574099349831925900000000,
      0.6129795430571779400000000,
      0.3362159841669621300000000,
      0.1512517303224336400000000,
      -0.1794156259657768100000000,
      -0.3503423610057745400000000,
      -0.2778372483233216800000000,
      -0.1592734122665366000000000
    )

    val Ismoke = Seq(
      0,
      0.2119377108760385200000000,
      0.6618634379685941500000000,
      0.7570714587132305600000000,
      0.9496298251457036000000000
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    var dage: Double = age
    dage = dage / 10
    var age_2: Double = dage
    var age_1 = pow(dage,.5)
    var dbmi: Double = bmi
    dbmi = dbmi / 10
    var bmi_2 = pow(dbmi,-2) * scala.math.log(dbmi)
    var bmi_1 = pow(dbmi,-2)

    /* Centring the continuous variables */

    age_1 = age_1 - 2.086397409439087
    age_2 = age_2 - 4.353054523468018
    bmi_1 = bmi_1 - 0.152244374155998
    bmi_2 = bmi_2 - 0.143282383680344
    val _rati = rati - 3.506655454635620
    val _sbp = sbp - 125.040039062500000
    val _town = town - 0.416743695735931

    /* Start of Sum */
    var a: Double = 0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * 4.4417863976316578000000000
    a += age_2 * 0.0281637210672999180000000
    a += bmi_1 * 0.8942365304710663300000000
    a += bmi_2 * -6.5748047596104335000000000
    a += _rati * 0.1433900561621420900000000
    a += _sbp * 0.0128971795843613720000000
    a += _town * 0.0664772630011438850000000

    /* Sum from boolean values */

    a += b_AF * 1.6284780236484424000000000
    a += b_ra * 0.2901233104088770700000000
    a += b_renal * 1.0043796680368302000000000
    a += b_treatedhyp * 0.6180430562788129500000000
    a += b_type1 * 1.8400348250874599000000000
    a += b_type2 * 1.1711626412196512000000000
    a += fh_cvd * 0.5147261203665195500000000

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat==1) 0.7464406144391666500000000 else 0)
    a += age_1 * (if (smoke_cat==2) 0.2568541711879666600000000 else 0)
    a += age_1 * (if (smoke_cat==3) -1.5452226707866523000000000 else 0)
    a += age_1 * (if (smoke_cat==4) -1.7113013709043405000000000 else 0)
    a += age_1 * b_AF * -7.0177986441269269000000000
    a += age_1 * b_renal * -2.9684019256454390000000000
    a += age_1 * b_treatedhyp * -4.2219906452967848000000000
    a += age_1 * b_type1 * 1.6835769546040080000000000
    a += age_1 * b_type2 * -2.9371798540034648000000000
    a += age_1 * bmi_1 * 0.1797196207044682300000000
    a += age_1 * bmi_2 * 40.2428166760658140000000000
    a += age_1 * fh_cvd * 0.1439979240753906700000000
    a += age_1 * _sbp * -0.0362575233899774460000000
    a += age_1 * _town * 0.3735138031433442600000000
    a += age_2 * (if (smoke_cat==1) -0.1927057741748231000000000 else 0)
    a += age_2 * (if (smoke_cat==2) -0.1526965063458932700000000 else 0)
    a += age_2 * (if (smoke_cat==3) 0.2313563976521429400000000 else 0)
    a += age_2 * (if (smoke_cat==4) 0.2307165013868296700000000 else 0)
    a += age_2 * b_AF * 1.1395776028337732000000000
    a += age_2 * b_renal * 0.4356963208330940600000000
    a += age_2 * b_treatedhyp * 0.7265947108887239600000000
    a += age_2 * b_type1 * -0.6320977766275653900000000
    a += age_2 * b_type2 * 0.4023270434871086800000000
    a += age_2 * bmi_1 * 0.1319276622711877700000000
    a += age_2 * bmi_2 * -7.3211322435546409000000000
    a += age_2 * fh_cvd * -0.1330260018273720400000000
    a += age_2 * _sbp * 0.0045842850495397955000000
    a += age_2 * _town * -0.0952370300845990780000000

    /* Calculate the score itself */
    100.0 * (1 - pow(survivor(surv), exp(a)))
  }


}
