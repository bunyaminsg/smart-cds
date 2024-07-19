package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir.{Condition, FamilyMemberHistory, MedicationStatement, Observation, Patient}
import srdc.smartcds.util.{DateTimeUtil, FhirParseHelper, UnitConceptEnum}

import scala.math.{exp, pow, sqrt}
import scala.util.Try

object QRisk3FlowExecution {

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
    val bmi = if (bmiOpt.isDefined) { bmiOpt.get } else { 25 }
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

    val smoking = if (smokingObs.isDefined) {
      smokingObs.get.valueCodeableConcept.get.coding.map(_.code).toSeq
    } else {Seq("266919005")}
    val smoke_cat = if (Seq("LA18978-9", "LA18980-5", "266919005").intersect(smoking).nonEmpty) 0
    else if (smoking.contains("LA15920-4", "8517006")) 1
    else if (Seq("LA18977-1", "LA18982-1").intersect(smoking).nonEmpty) 2
    else if (Seq("LA18979-7", "LA18976-3", "449868002").intersect(smoking).nonEmpty) 3
    else if (smoking.contains("LA18981-3")) 4
    else 0

    val surv = 10
    val town = 0
    val ethrisk = 0
    //val ethriskCode = Ethnicity.head.valueCodeableConcept.flatMap(_.coding.headOption.map(_.code)).getOrElse("0")

/*    val ethrisk: Int = ethriskCode match {
      case "0" => 1
      case "1" => 2
      case "2" => 3
      case "3" => 4
      case "4" => 5
      case "5" => 6
      case "6" => 7
      case "7" => 8
      case "8" => 9
      case _ => 0
    }*/

    if (patient.gender.contains("male")) {
      val patientScore = cvdMaleRaw(age, b_AF, b_atypicalantipsy, b_corticosteroids, b_impotence2, b_migraine, b_ra, b_renal, b_semi, b_sle, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpDeviation, smoke_cat, surv, town)
      val healthyScore = cvdMaleRaw(age, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 4, 125, 0, 0, surv, town)
      Some(patientScore,healthyScore)
    } else {
      val patientScore = cvdFemaleRaw(age, b_AF, b_atypicalantipsy, b_corticosteroids, b_migraine, b_ra, b_renal, b_semi, b_sle, b_treatedhyp, b_type1, b_type2, bmi, ethrisk, fh_cvd, rati, sbp, sbpDeviation, smoke_cat, surv, town)
      val healthyScore = cvdFemaleRaw(age, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, ethrisk, 0, 4, 125, 0, 0, surv, town)
      Some(patientScore,healthyScore)
    }
  }
  def cvdMaleRaw(
                  age: Int, b_AF: Int, b_atypicalantipsy: Int, b_corticosteroids: Int, b_impotence2: Int, b_migraine: Int, b_ra: Int, b_renal: Int, b_semi: Int, b_sle: Int, b_treatedhyp: Int, b_type1: Int, b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, sbps5: Double, smoke_cat: Int, surv: Int, town: Double
                ): Double = {

    val survivor: Seq[Double] = Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.977268040180206, 0, 0, 0, 0, 0)


    /* The conditional arrays */

    val Iethrisk: Seq[Double] = Seq(
      0,
      0,
      0.2771924876030827900000000,
      0.4744636071493126800000000,
      0.5296172991968937100000000,
      0.0351001591862990170000000,
      -0.3580789966932791900000000,
      -0.4005648523216514000000000,
      -0.4152279288983017300000000,
      -0.2632134813474996700000000
    )

    val Ismoke: Seq[Double] = Seq(
      0,
      0.1912822286338898300000000,
      0.5524158819264555200000000,
      0.6383505302750607200000000,
      0.7898381988185801900000000
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    var dage: Double = age
    dage = dage / 10
    var age_1 = pow(dage,-1)
    var age_2 = pow(dage,3)
    var dbmi: Double = bmi
    dbmi = dbmi / 10
    var bmi_2 = pow(dbmi,-2)*scala.math.log(dbmi)
    var bmi_1 = pow(dbmi,-2)

    /* Centring the continuous variables */

    age_1 = age_1 - 0.234766781330109
    age_2 = age_2 - 77.284080505371094
    bmi_1 = bmi_1 - 0.149176135659218
    bmi_2 = bmi_2 - 0.141913309693336
    val _rati = rati - 4.300998687744141
    val _sbp = sbp - 128.571578979492190
    val _sbps5 = sbps5 - 8.756621360778809
    val _town = town - 0.526304900646210

    /* Start of Sum */
    var a: Double = 0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * -17.8397816660055750000000000
    a += age_2 * 0.0022964880605765492000000
    a += bmi_1 * 2.4562776660536358000000000
    a += bmi_2 * -8.3011122314711354000000000
    a += _rati * 0.1734019685632711100000000
    a += _sbp * 0.0129101265425533050000000
    a += _sbps5 * 0.0102519142912904560000000
    a += _town * 0.0332682012772872950000000

    /* Sum from boolean values */

    a += b_AF * 0.8820923692805465700000000
    a += b_atypicalantipsy * 0.1304687985517351300000000
    a += b_corticosteroids * 0.4548539975044554300000000
    a += b_impotence2 * 0.2225185908670538300000000
    a += b_migraine * 0.2558417807415991300000000
    a += b_ra * 0.2097065801395656700000000
    a += b_renal * 0.7185326128827438400000000
    a += b_semi * 0.1213303988204716400000000
    a += b_sle * 0.4401572174457522000000000
    a += b_treatedhyp * 0.5165987108269547400000000
    a += b_type1 * 1.2343425521675175000000000
    a += b_type2 * 0.8594207143093222100000000
    a += fh_cvd * 0.5405546900939015600000000

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat == 1) -0.2101113393351634600000000 else 0)
    a += age_1 * (if (smoke_cat == 2) 0.7526867644750319100000000 else 0)
    a += age_1 * (if (smoke_cat == 3) 0.9931588755640579100000000 else 0)
    a += age_1 * (if (smoke_cat == 4) 2.1331163414389076000000000 else 0)
    a += age_1 * b_AF * 3.4896675530623207000000000
    a += age_1 * b_corticosteroids * 1.1708133653489108000000000
    a += age_1 * b_impotence2 * -1.5064009857454310000000000
    a += age_1 * b_migraine * 2.3491159871402441000000000
    a += age_1 * b_renal * -0.5065671632722369400000000
    a += age_1 * b_treatedhyp * 6.5114581098532671000000000
    a += age_1 * b_type1 * 5.3379864878006531000000000
    a += age_1 * b_type2 * 3.6461817406221311000000000
    a += age_1 * bmi_1 * 31.0049529560338860000000000
    a += age_1 * bmi_2 * -111.2915718439164300000000000
    a += age_1 * fh_cvd * 2.7808628508531887000000000
    a += age_1 * sbp * 0.0188585244698658530000000
    a += age_1 * town * -0.1007554870063731000000000
    a += age_2 * (if (smoke_cat == 1) -0.0004985487027532612100000 else 0)
    a += age_2 * (if (smoke_cat == 2) -0.0007987563331738541400000 else 0)
    a += age_2 * (if (smoke_cat == 3) -0.0008370618426625129600000 else 0)
    a += age_2 * (if (smoke_cat == 4) -0.0007840031915563728900000 else 0)
    a += age_2 * b_AF * -0.0003499560834063604900000
    a += age_2 * b_corticosteroids * -0.0002496045095297166000000
    a += age_2 * b_impotence2 * -0.0011058218441227373000000
    a += age_2 * b_migraine * 0.0001989644604147863100000
    a += age_2 * b_renal * -0.0018325930166498813000000
    a += age_2 * b_treatedhyp * 0.0006383805310416501300000
    a += age_2 * b_type1 * 0.0006409780808752897000000
    a += age_2 * b_type2 * -0.0002469569558886831500000
    a += age_2 * bmi_1 * 0.0050380102356322029000000
    a += age_2 * bmi_2 * -0.0130744830025243190000000
    a += age_2 * fh_cvd * -0.0002479180990739603700000
    a += age_2 * sbp * -0.0000127187419158845700000
    a += age_2 * town * -0.0000932996423232728880000

    /* Calculate the score itself */
    100.0 * (1 - pow(survivor(surv), exp(a)))
  }



  def cvdFemaleRaw(
                    age: Int, b_AF: Int, b_atypicalantipsy: Int, b_corticosteroids: Int, b_migraine: Int, b_ra: Int, b_renal: Int, b_semi: Int, b_sle: Int, b_treatedhyp: Int, b_type1: Int, b_type2: Int, bmi: Double, ethrisk: Int, fh_cvd: Int, rati: Double, sbp: Double, sbps5: Double, smoke_cat: Int, surv: Int, town: Double
                  ): Double = {

    val survivor = Array[Double](
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
      0.988876402378082,
      0,
      0,
      0,
      0,
      0
    )

    /* The conditional arrays */

    val Iethrisk = Array[Double](
      0,
      0,
      0.2804031433299542500000000,
      0.5629899414207539800000000,
      0.2959000085111651600000000,
      0.0727853798779825450000000,
      -0.1707213550885731700000000,
      -0.3937104331487497100000000,
      -0.3263249528353027200000000,
      -0.1712705688324178400000000
    )

    val Ismoke = Array[Double](
      0,
      0.1338683378654626200000000,
      0.5620085801243853700000000,
      0.6674959337750254700000000,
      0.8494817764483084700000000
    )

    /* Applying the fractional polynomial transforms */
    /* (which includes scaling)                      */

    val dage = age.toDouble / 10
    var age_1 = Math.pow(dage, -2)
    var age_2 = dage
    val dbmi = bmi / 10
    var bmi_1 = Math.pow(dbmi, -2)
    var bmi_2 = Math.pow(dbmi, -2) * Math.log(dbmi)

    /* Centring the continuous variables */

    age_1 -= 0.053274843841791
    age_2 -= 4.332503318786621
    bmi_1 -= 0.154946178197861
    bmi_2 -= 0.144462317228317
    val ratiVal = rati - 4.300998687744141
    val sbpVal = sbp - 128.571578979492190
    val sbps5Val = sbps5 - 8.756621360778809
    val townVal = town - 0.526304900646210

    /* Start of Sum */
    var a = 0.0

    /* The conditional sums */

    a += Iethrisk(ethrisk)
    a += Ismoke(smoke_cat)

    /* Sum from continuous values */

    a += age_1 * -8.1388109247726188000000000
    a += age_2 * 0.7973337668969909800000000
    a += bmi_1 * 0.2923609227546005200000000
    a += bmi_2 * -4.1513300213837665000000000
    a += ratiVal * 0.1533803582080255400000000
    a += sbpVal * 0.0131314884071034240000000
    a += sbps5Val * 0.0078894541014586095000000
    a += townVal * 0.0772237905885901080000000

    /* Sum from boolean values */

    a += b_AF * 1.5923354969269663000000000
    a += b_atypicalantipsy * 0.2523764207011555700000000
    a += b_corticosteroids * 0.5952072530460185100000000
    a += b_migraine * 0.3012672608703450000000000
    a += b_ra * 0.2136480343518194200000000
    a += b_renal * 0.6519456949384583300000000
    a += b_semi * 0.1255530805882017800000000
    a += b_sle * 0.7588093865426769300000000
    a += b_treatedhyp * 0.5093159368342300400000000
    a += b_type1 * 1.7267977510537347000000000
    a += b_type2 * 1.0688773244615468000000000
    a += fh_cvd * 0.4544531902089621300000000

    /* Sum from interaction terms */

    a += age_1 * (if (smoke_cat == 1) -4.7057161785851891000000000 else 0)
    a += age_1 * (if (smoke_cat == 2) -2.7430383403573337000000000 else 0)
    a += age_1 * (if (smoke_cat == 3) -0.8660808882939218200000000 else 0)
    a += age_1 * (if (smoke_cat == 4) 0.9024156236971064800000000 else 0)
    a += age_1 * (if (b_AF == 1) 19.9380348895465610000000000 else 0)
    a += age_1 * (if (b_corticosteroids == 1) -0.9840804523593628100000000 else 0)
    a += age_1 * (if (b_migraine == 1) 1.7634979587872999000000000 else 0)
    a += age_1 * (if (b_renal == 1) -3.5874047731694114000000000 else 0)
    a += age_1 * (if (b_sle == 1) 19.6903037386382920000000000 else 0)
    a += age_1 * (if (b_treatedhyp == 1) 11.8728097339218120000000000 else 0)
    a += age_1 * (if (b_type1 == 1) -1.2444332714320747000000000 else 0)
    a += age_1 * (if (b_type2 == 1) 6.8652342000009599000000000 else 0)
    a += age_1 * bmi_1 * 23.8026234121417420000000000
    a += age_1 * bmi_2 * -71.1849476920870070000000000
    a += age_1 * (if (fh_cvd == 1) 0.9946780794043512700000000 else 0)
    a += age_1 * sbp * 0.0341318423386154850000000
    a += age_1 * town * -1.0301180802035639000000000
    a += age_2 * (if (smoke_cat == 1) -0.0755892446431930260000000 else 0)
    a += age_2 * (if (smoke_cat == 2) -0.1195119287486707400000000 else 0)
    a += age_2 * (if (smoke_cat == 3) -0.1036630639757192300000000 else 0)
    a += age_2 * (if (smoke_cat == 4) -0.1399185359171838900000000 else 0)
    a += age_2 * (if (b_AF == 1) -0.0761826510111625050000000 else 0)
    a += age_2 * (if (b_corticosteroids == 1) -0.1200536494674247200000000 else 0)
    a += age_2 * (if (b_migraine == 1) -0.0655869178986998590000000 else 0)
    a += age_2 * (if (b_renal == 1) -0.2268887308644250700000000 else 0)
    a += age_2 * (if (b_sle == 1) 0.0773479496790162730000000 else 0)
    a += age_2 * (if (b_treatedhyp == 1) 0.0009685782358817443600000 else 0)
    a += age_2 * (if (b_type1 == 1) -0.2872406462448894900000000 else 0)
    a += age_2 * (if (b_type2 == 1) -0.0971122525906954890000000 else 0)
    a += age_2 * bmi_1 * 0.5236995893366442900000000
    a += age_2 * bmi_2 * 0.0457441901223237590000000
    a += age_2 * (if (fh_cvd == 1) -0.0768850516984230380000000 else 0)
    a += age_2 * sbp * -0.0015082501423272358000000
    a += age_2 * town * -0.0315934146749623290000000

    /* Calculate the score itself */
    val score = 100.0 * (1 - Math.pow(survivor(surv), Math.exp(a)))
    score
  }
}
