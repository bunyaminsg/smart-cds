package srdc.smartcds.cds.flow

import io.onfhir.cds.model.CdsResponseBuilder
import srdc.smartcds.model.fhir.{Observation, Patient}
import srdc.smartcds.util.{ConceptIdUtil, FhirParseHelper, ValueSetUtil}

object Score2FlowExecution {

  /**
   * Execute SCORE2 Calculation flow
   * @param patient Patient resource
   * @param totalCholesterol Total Cholesterol Observation
   * @param hdl HDL Observation
   * @param bp Blood Pressure Observation
   * @param smokingStatus Smoking Status Observation
   * @param region Region Observation
   * @param responseBuilder Response Builder
   * @return if applicable, returns the related SCORE2 Card and the (x,y) index of patient's score on the card; else 'no-value' or 'missing-input'
   */
  def executeFlow(patient: Patient, totalCholesterol: Seq[Observation], hdl: Seq[Observation], bp: Seq[Observation], smokingStatus: Seq[Observation], region: Seq[Observation], responseBuilder: CdsResponseBuilder) = {
    println(s"Executing SCORE2 algorithm...")

    val age = FhirParseHelper.getAge(patient)
    val isFemale = patient.gender.contains("female")

    val sbpObs = FhirParseHelper.findLatestObservation(ValueSetUtil.getConceptSystemAndCode(ConceptIdUtil.SYSTOLIC_BP), bp)
    val bpObs = FhirParseHelper.findLatestObservation(ValueSetUtil.getConceptSystemAndCode(ConceptIdUtil.BP), bp)

    val sbpOpt = if ((sbpObs.isEmpty || !FhirParseHelper.checkObservationValuesExist(List(sbpObs))) && bpObs.nonEmpty) {
      val sbpComp = bpObs.get.component.flatMap(_.find(comp =>
        FhirParseHelper.checkCodingMatch(comp.code, ValueSetUtil.getConceptSystemAndCode(ConceptIdUtil.SYSTOLIC_BP), startsWith = true)))
      if (sbpComp.nonEmpty && sbpComp.get.valueQuantity.flatMap(_.value).nonEmpty) {
        sbpComp.get.valueQuantity.get.value
      } else {
        None
      }
    } else {
      FhirParseHelper.getQuantityObservationValue(sbpObs, None)
    }

    if (smokingStatus.isEmpty || sbpOpt.isEmpty || totalCholesterol.isEmpty || hdl.isEmpty || region.isEmpty) {
      // Cannot be calculated
      Right("missing-input")
    } else if (age < 40 || age > 89) {
      // Score value does not exist
      Right("no-value")
    } else {
      /**
       * Imagine the card code as three-digit number
       * First digit represents the gender: 1 for women, 2 for men
       * Second digit represents the smoking status: 1 for non-smoking, 2 for smoking
       * Last digit represents the age range: 0 for 85-89, 1 for 80-84, 2 for 75-79, 3 for 70-74,
       *  4 for 65-69, 5 for 60-64, 6 for 55-59, 7 for 50-54, 8 for 45-49, 9 for 40-44
       *
       *  For example,
       *  cardCode of a patient female, smoking and age 57 will be 126
       *  cardCode of a patient male, non-smoking and age 42 will be 219
       *  cardCode of a patient female, non-smoking and age 72 will be 113
       *  and so on.
       */
      var cardCode = 0
      if (isFemale)
        cardCode = 100
      else
        cardCode = 200

      if (smokingStatus.headOption.exists(_.valueCodeableConcept.exists(_.coding.exists(_.code match {
        case "LA18976-3" | "LA18979-7" | "LA18977-1" | "LA18982-1" | "LA18981-3" => true
        case _ => false
      }))))
        cardCode += 20
      else
        cardCode += 10

      if(age >= 85 && age <= 89) cardCode += 0
      if(age >= 80 && age <= 84) cardCode += 1
      if(age >= 75 && age <= 79) cardCode += 2
      if(age >= 70 && age <= 74) cardCode += 3
      if(age >= 65 && age <= 69) cardCode += 4
      if(age >= 60 && age <= 64) cardCode += 5
      if(age >= 55 && age <= 59) cardCode += 6
      if(age >= 50 && age <= 54) cardCode += 7
      if(age >= 45 && age <= 49) cardCode += 8
      if(age >= 40 && age <= 44) cardCode += 9

      println(s"Card code is $cardCode")

      val regionCode = region.head.valueCodeableConcept.flatMap(_.coding.headOption.map(_.code)).getOrElse("0")

      var card: String = regionCode match {
        case "0" => getSCORE2CardModerateRiskRegion(cardCode)
        case "1" => getSCORE2CardModerateRiskRegion(cardCode)
        case _ => getSCORE2CardHighRiskRegion(cardCode)
      }

      val scoreValue = findScoreValueOnCard(sbpOpt.get, totalCholesterol.head, hdl.head, card)

      if (scoreValue.nonEmpty) {
        Left(scoreValue.get._1, scoreValue.get._2, card)
      } else {
        // Score value does not exist
        Right("no-value")
      }
    }
  }

  /**
   * @param cardCode Specific code calculated according to patient's data
   * @return The suitable SCORE2 Card for High Risk European Regions
   */
  private def getSCORE2CardHighRiskRegion(cardCode: Int): String = {
    println(s"Generating SCORE2 card for high risk region...")
    val card = cardCode match {
      // Women, non-smoking
      case 110 => "53,55,57,58|50,52,54,55|47,49,51,52|44,46,48,50"
      case 111 => "40,42,44,45|36,38,39,41|32,34,36,37|29,31,32,34"
      case 112 => "29,31,32,34|25,27,28,29|22,23,24,25|18,19,20,22"
      case 113 => "21,22,24,25|17,18,19,20|14,15,16,17|11,12,13,14"
      case 114 => "15,16,17,18|12,13,14,14|10,10,11,11|8,8,8,9"
      case 115 => "11,11,12,13|8,9,9,10|6,7,7,8|5,5,6,6"
      case 116 => "7,8,9,10|5,6,7,7|4,4,5,5|3,3,4,4"
      case 117 => "5,5,6,7|3,4,4,5|3,3,3,4|2,2,2,3"
      case 118 => "3,4,4,5|2,3,3,4|2,2,2,2|1,1,2,2"
      case 119 => "2,3,3,4|1,2,2,2|1,1,1,2|1,1,1,1"
      // Women, smoking
      case 120 => "58,59,61,63|55,56,58,60|52,53,55,57|49,51,52,54"
      case 121 => "49,51,54,55|44,46,48,50|40,42,44,46|36,38,40,41"
      case 122 => "41,43,45,47|35,37,39,41|31,32,34,36|26,28,29,31"
      case 123 => "33,35,37,39|28,29,31,33|23,24,26,27|19,20,21,22"
      case 124 => "26,27,29,30|21,22,23,24|16,17,18,19|13,14,14,15"
      case 125 => "20,21,23,25|15,16,18,19|12,13,14,15|9,10,11,11"
      case 126 => "15,16,18,20|11,12,14,15|8,9,10,11|6,7,8,8"
      case 127 => "11,13,14,16|8,9,10,12|6,7,8,9|4,5,6,6"
      case 128 => "8,10,11,13|6,7,8,9|4,5,6,6|3,3,4,5"
      case 129 => "6,7,9,10|4,5,6,7|3,4,4,5|2,2,3,3"
      // Men, non-smoking
      case 210 => "42,49,57,65|40,47,55,63|38,45,53,61|36,43,51,58"
      case 211 => "34,40,45,51|31,36,42,47|29,33,38,44|26,30,35,40"
      case 212 => "28,32,35,39|24,27,31,34|21,24,27,30|18,20,23,26"
      case 213 => "23,25,27,29|19,20,22,24|15,17,18,20|12,14,15,16"
      case 214 => "17,18,20,22|14,15,16,18|11,12,13,15|9,10,11,12"
      case 215 => "13,14,16,18|10,11,13,14|8,9,10,11|6,7,8,9"
      case 216 => "9,11,12,14|7,8,10,11|6,6,7,9|4,5,6,7"
      case 217 => "7,8,10,11|5,6,7,9|4,5,5,6|3,3,4,5"
      case 218 => "5,6,8,9|4,5,6,7|3,3,4,5|2,2,3,4"
      case 219 => "4,5,6,7|3,3,4,5|2,2,3,4|1,2,2,3"
      // Men, smoking
      case 220 => "41,49,56,65|40,47,54,62|38,45,52,60|36,43,50,58"
      case 221 => "38,44,50,56|35,40,46,52|32,37,42,48|29,34,39,44"
      case 222 => "35,39,44,48|31,34,38,43|27,30,34,37|23,26,29,33"
      case 223 => "33,35,38,41|27,29,32,34|22,24,26,28|18,20,22,23"
      case 224 => "25,28,30,32|21,23,25,27|17,19,20,22|14,15,17,18"
      case 225 => "20,23,25,28|16,18,20,23|13,15,16,18|10,12,13,15"
      case 226 => "16,19,21,24|13,15,17,19|10,11,13,15|8,9,10,12"
      case 227 => "13,15,18,21|10,12,14,16|7,9,10,12|6,7,8,9"
      case 228 => "10,13,15,18|8,9,11,14|6,7,8,10|4,5,6,7"
      case 229 => "8,10,13,16|6,7,9,11|4,5,7,8|3,4,5,6"
    }

    println(s"Generated the card: ${card}")
    card
  }

  /**
   * @param cardCode Specific code calculated according to patient's data
   * @return The suitable SCORE2 Card for Moderate Risk European Regions
   */
  private def getSCORE2CardModerateRiskRegion(cardCode: Int): String = {
    println(s"Generating SCORE2 card for moderate risk region...")
    val card = cardCode match {
      // Women, non-smoking
      case 110 => "37,39,40,42|35,36,38,39|32,34,35,37|30,32,33,34"
      case 111 => "27,28,30,31|24,25,27,28|21,22,24,25|19,20,21,22"
      case 112 => "19,20,21,23|16,17,18,19|14,15,15,16|12,12,13,14"
      case 113 => "13,14,15,16|11,11,12,13|9,9,10,11|7,7,8,8"
      case 114 => "10,10,11,12|8,9,9,9|7,7,7,8|5,6,6,6"
      case 115 => "7,8,8,9|6,6,7,7|5,5,5,6|4,4,4,5"
      case 116 => "5,6,6,7|4,4,5,5|3,3,4,4|3,3,3,3"
      case 117 => "4,4,5,5|3,3,4,4|2,2,3,3|2,2,2,2"
      case 118 => "3,3,3,4|2,2,3,3|2,2,2,2|1,1,1,2"
      case 119 => "2,2,3,3|1,2,2,2|1,1,1,2|1,1,1,1"
      // Women, smoking
      case 120 => "41,43,44,46|39,40,42,43|36,38,39,41|34,35,37,38"
      case 121 => "34,35,37,39|30,32,33,35|27,28,30,31|24,25,27,28"
      case 122 => "27,29,30,32|24,25,26,28|20,21,22,24|17,18,19,20"
      case 123 => "22,23,25,26|18,19,20,22|15,16,17,18|12,13,13,14"
      case 124 => "15,16,17,18|13,13,14,15|10,11,12,12|9,9,9,10"
      case 125 => "12,13,14,15|10,11,11,12|8,8,9,10|6,7,7,8"
      case 126 => "10,11,11,12|8,8,9,10|6,7,7,8|5,5,6,6"
      case 127 => "8,8,9,10|6,6,7,8|5,5,6,6|3,4,4,5"
      case 128 => "6,7,8,9|5,5,6,6|3,4,4,5|3,3,3,4"
      case 129 => "5,5,6,7|3,4,5,5|3,3,3,4|2,2,2,3"
      // Men, non-smoking
      case 210 => "37,45,53,62|36,43,51,59|34,41,49,57|32,39,47,55"
      case 211 => "30,35,41,47|27,32,37,43|25,29,34,40|22,26,31,36"
      case 212 => "24,27,31,35|21,23,27,30|17,20,23,26|15,17,19,22"
      case 213 => "19,21,23,25|15,17,18,20|12,13,15,16|10,11,12,13"
      case 214 => "14,15,17,18|12,13,14,15|10,11,12,13|8,9,10,10"
      case 215 => "11,12,13,15|9,10,11,12|7,8,9,10|6,7,7,8"
      case 216 => "9,10,11,12|7,8,9,10|5,6,7,8|4,5,6,6"
      case 217 => "7,8,9,10|5,6,7,8|4,5,5,6|3,4,4,5"
      case 218 => "5,6,7,8|4,5,5,6|3,4,4,5|2,3,3,4"
      case 219 => "4,5,6,7|3,4,4,5|2,3,3,4|2,2,2,3"
      // Men, smoking
      case 220 => "37,45,53,61|35,43,51,59|34,41,48,57|32,39,46,55"
      case 221 => "34,40,46,53|31,36,42,48|28,33,38,44|25,30,35,40"
      case 222 => "31,35,39,44|27,30,34,38|23,26,29,33|19,22,25,29"
      case 223 => "28,31,34,36|23,25,28,30|19,20,22,24|15,16,18,20"
      case 224 => "20,22,23,25|17,18,20,21|14,15,17,18|12,13,14,15"
      case 225 => "17,18,20,22|14,15,17,18|11,13,14,15|9,10,11,12"
      case 226 => "14,16,17,20|11,13,14,16|9,10,11,13|7,8,9,10"
      case 227 => "11,13,15,17|9,10,12,14|7,8,9,11|5,6,7,8"
      case 228 => "9,11,13,15|7,8,10,12|5,7,8,9|4,5,6,7"
      case 229 => "8,9,11,13|6,7,8,10|4,5,6,8|3,4,5,6"
    }

    println(s"Generated the card: ${card}")
    card
  }

  /**
   * @param systolicValue Systolic BP value
   * @param totalCholesterol Total Cholesterol Value
   * @param hdl HDL Value
   * @param card SCORE2 Card
   * @return The index of the patient's score on the related SCORE2 card
   */
  private def findScoreValueOnCard(systolicValue: Double, totalCholesterol: Observation, hdl: Observation, card: String): Option[(String, String)] = {
    println(s"Finding score value on card...")
    println(s"Systolic BP value is ${systolicValue} mmHg")

    val nonHDLValue = totalCholesterol.valueQuantity.get.value.get - hdl.valueQuantity.get.value.get
    println(s"Non-HDL value is ${nonHDLValue} mg/dL")

    var nonHDLValueMMOL_L = nonHDLValue / 38.67 // for converting value from mg/dL to mmol/L
    println(s"Converted non-HDL value is ${nonHDLValueMMOL_L} mmol/L")
    nonHDLValueMMOL_L = BigDecimal(nonHDLValueMMOL_L).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble // Then convert it to a double with one decimal point
    println(s"Rounded non-HDL value is ${nonHDLValueMMOL_L} mmol/L")

    var xIndex = -1
    var yIndex = -1

    if (systolicValue >= 160/* && systolicValue <= 179*/) xIndex = 0
    else if (systolicValue >= 140 && systolicValue <= 159) xIndex = 1
    else if (systolicValue >= 120 && systolicValue <= 139) xIndex = 2
    else if (systolicValue >= 100 && systolicValue <= 119) xIndex = 3

    if(nonHDLValueMMOL_L >= 3.0 && nonHDLValueMMOL_L <= 3.9) yIndex = 0
    else if(nonHDLValueMMOL_L >= 4.0 && nonHDLValueMMOL_L <= 4.9) yIndex = 1
    else if(nonHDLValueMMOL_L >= 5.0 && nonHDLValueMMOL_L <= 5.9) yIndex = 2
    else if(nonHDLValueMMOL_L >= 6.0/* && nonHDLValueMMOL_L <= 6.9*/) yIndex = 3

    if (xIndex >= 0 && yIndex >= 0) {
      val rows: Array[String] = card.split("\\|")
      val row = rows(xIndex)

      val columns: Array[String] = row.split(",")
      val value = columns(yIndex)

      println(s"Value at row #${xIndex} and column #${yIndex} is ${value}")
      Some((value, s"${xIndex}|${yIndex}"))
    } else {
      None
    }
  }

}
