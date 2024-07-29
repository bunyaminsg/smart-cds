package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.util.DateTimeUtil


/*
This work utilizes the Advanced Cardiovascular Risk Model as described in the article
"Advanced cardiovascular risk prediction in the emergency department: updating a clinical prediction model
– a large database study protocol," which is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0).

License Link: CC BY 4.0

Source: "Advanced cardiovascular risk prediction in the emergency department: updating a clinical prediction model
– a large database study protocol." Published in Diagnostic and Prognostic Research.
Available at: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3902381/

 */

object AdvanceFlowExecution {
  /**
   * Execute ADVANCE Calculation flow
   *
   * @param age
   * @param gender
   * @param atrialFibrillation
   * @param retinopathy
   * @param hypertensiveTreatment
   * @param hba1cOpt
   * @param acrOpt
   * @param totalCholesterolOpt
   * @param hdlOpt
   * @param sbpOpt
   * @param dbpOpt
   * @param type1Diabetes
   * @param type2Diabetes
   * @param type1DiabetesDuration
   * @param type2DiabetesDuration
   * @param ckd45
   * @param cvd
   * @param atorvastatin
   * @param responseBuilder
   * @return
   */

  def executionFlow(age: Int, gender: String, atrialFibrillation: Boolean,retinopathy: Boolean, hypertensiveTreatment: Boolean,
                    hba1cOpt: Option[Double], acrOpt: Option[Double], totalCholesterolOpt: Option[Double], hdlOpt: Option[Double],
                    sbpOpt: Option[Double], dbpOpt: Option[Double], type1Diabetes: Boolean, type2Diabetes: Boolean, type1DiabetesDuration: Option[Int],
                    type2DiabetesDuration: Option[Int], ckd45: Boolean, cvd: Boolean, atorvastatin: Boolean,
                    responseBuilder: CdsResponseBuilder): CdsResponseBuilder = {

    val isFemale = (gender == "female")


    var nonhdl = 0.0

    var hdl= 0.0
    if(hdlOpt.isDefined){
      hdl = hdlOpt.get
    } else {
      hdl = 1.6
    }
    var cholesterol = 0.0
    if(totalCholesterolOpt.isDefined){
      cholesterol = totalCholesterolOpt.get
    } else {
      cholesterol = 5.17
    }

    nonhdl = cholesterol - hdl

    //Get Albumin/Creatine Ratio Observation
    var acr = 0.0
    if(acrOpt.isDefined){
      acr = acrOpt.get
    } else {acr = 2.0}

    //Get Hba1c Observation
    var hba1c = 0.0
    if(hba1cOpt.isDefined){
      hba1c = hba1cOpt.get
    } else {hba1c = 39}



    //Get blood pressure observations and calculate pulse pressure
    var sbp = 120.0
    var dbp = 80.0
    if(sbpOpt.isDefined){
      sbp = sbpOpt.get
    }
    if(dbpOpt.isDefined) {
      dbp = dbpOpt.get
    }
    val pulse_p = sbp - dbp

    //Get boolean type parameters



    //Get diabetes observation and calculate diabetes duration and age
    var diabetes_duration = -1
    var diabetes_age = 0
    if(type1Diabetes && type2Diabetes){
      var type1_duration = 0
      var type2_duration = 0
      if(type1DiabetesDuration.isDefined){
        type1_duration = type1DiabetesDuration.get
      }
      if(type2DiabetesDuration.isDefined){
        type2_duration = type2DiabetesDuration.get
      }
      if( type1_duration > type2_duration) {
        diabetes_duration = type1_duration
      } else {
        diabetes_duration = type2_duration
      }
      diabetes_age = age - diabetes_duration
    }
    else if(type1Diabetes){
      if(type1DiabetesDuration.isDefined){
        diabetes_duration = type1DiabetesDuration.get
      } else {
        diabetes_duration = 0
      }
      diabetes_age = age - diabetes_duration
    }
    else if(type2Diabetes) {
      if(type2DiabetesDuration.isDefined){
        diabetes_duration = type2DiabetesDuration.get
      } else {
        diabetes_duration = 0
      }
      diabetes_age = age - diabetes_duration
    }


    //Initialize point summations
    var sum = 0
    var healthy_sum = 0


    //Check each of the values and accumulate points corresponding to the parameter values

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

    if(retinopathy) {sum += 1}

    if(hypertensiveTreatment) {sum += 1}

    if(atrialFibrillation) {sum += 2}

    if(hba1c >= 75) {
      sum += 2
    } else if(hba1c >= 42) {
      sum += 1
    }

    if(nonhdl>=9) {
      sum += 5
    } else if(nonhdl >= 6){
      sum += 2
    } else if(nonhdl >= 3){
      sum += 1
    }


    if(acr>30) {
      sum += 3
    } else if(acr >= 3){
      sum +=2
    }

    //Get risk percentages corresponding to the accumulated risk points
    val risk_score = mapResult(sum)
    val healthy_score = mapResult(healthy_sum)


    var output = responseBuilder
    output = output.withCard(_.loadCardWithPostTranslation("card-score",
      "effectiveDate" -> DateTimeUtil.zonedNow(),
      "scoreValue" -> risk_score,
      "healthyValue" -> healthy_score
    ))

    output = recommendReduceACRIfApplicable(acrOpt, output)
    output = recommendReduceHBA1CIfApplicable(hba1cOpt, output)
    output = recommendAtorvastatinIfApplicable(risk_score, type2Diabetes, ckd45, cvd, atorvastatin, hdlOpt, totalCholesterolOpt, output)
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
        case _ => 84
      }
    }
    result
  }

  /**
   * Recommends ACR Reduction if needed
   *
   * @param acrOpt
   * @param output
   * @return
   */
  private def recommendReduceACRIfApplicable(acrOpt: Option[Double], output: CdsResponseBuilder) = {
    if(acrOpt.isDefined){
      val acr = acrOpt.get
      if(acr > 30) {
        output.withCard(_.loadCardWithPostTranslation("card-reduce-acr",
          "effectiveDate" -> DateTimeUtil.zonedNow()
        ))
      } else {
        output
      }
    } else {
      output
    }
  }


  /**
   * Recommends HbA1C Reduction if needed
   *
   * @param hba1cOpt
   * @param output
   * @return
   */
  private def recommendReduceHBA1CIfApplicable(hba1cOpt: Option[Double], output: CdsResponseBuilder) = {
    if(hba1cOpt.isDefined){
      val hba1c = hba1cOpt.get
      if(hba1c >= 42) {
        output.withCard(_.loadCardWithPostTranslation("card-reduce-hba1c",
          "effectiveDate" -> DateTimeUtil.zonedNow()
        ))
      } else {
        output
      }
    } else {
      output
    }
  }

  /**
   * Recommends ACR Reduction if needed
   *
   * @param riskScore Risk Score Calculated So Far
   * @param t2d Type2Diabetes Observation
   * @param ckd Stage 4 or 5 Chronic Kidney Disease Observation
   * @param cvd CVD Observation
   * @param atorvastatin Atorvastatin Medication
   * @param hdl HDL Observation
   * @param cholesterol TotalCholesterol Observation
   * @param output CdsResponseBuilder object
   * @return output with updated card
   */
  private def recommendAtorvastatinIfApplicable(riskScore: Double, t2d: Boolean, ckd: Boolean, cvd: Boolean, atorvastatin: Boolean,
                                                hdl: Option[Double], cholesterol: Option[Double], output: CdsResponseBuilder): CdsResponseBuilder = {
    if(cholesterol.isDefined && hdl.isDefined){
      val _cholesterol = cholesterol.get
      val _hdl = hdl.get
      if((_cholesterol - _hdl)>3) {
        val targetCholesterol = _cholesterol - ((_cholesterol - _hdl) * 0.4)
        if (t2d && ckd && !atorvastatin) {
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

    } else {
      output
    }

  }
}
