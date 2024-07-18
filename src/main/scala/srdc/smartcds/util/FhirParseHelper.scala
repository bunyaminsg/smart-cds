package srdc.smartcds.util

import srdc.smartcds.model.fhir._
import srdc.smartcds.util.UnitConceptEnum.UnitConceptEnum

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Period}

object FhirParseHelper {

  /**
   * Check if the given codeable concept includes one of the given codes
   * @param codeableConcept Source CodeableConcept
   * @param systemCodes Target Codes
   * @param startsWith Check startsWith or exact match
   * @return
   */
  def checkCodingMatch(codeableConcept: CodeableConcept, systemCodes: List[(String, String)], startsWith: Boolean): Boolean = {
    codeableConcept.coding.foreach(coding => {
      systemCodes.foreach(systemCode => {
        if (startsWith) {
          if (coding.code.startsWith(systemCode._2) && (coding.system.isDefined && coding.system.get == systemCode._1)) {
            return true
          }
        } else {
          if (coding.code == systemCode._2 && (coding.system.isDefined && coding.system.get == systemCode._1)) {
            return true;
          }
        }

      })
    })
    false
  }

  /**
   * @param patient Patient
   * @return age of the patient
   */
  def getAge(patient: Patient): Int = {
    Period.between(LocalDate.parse(patient.birthDate.get, DateTimeFormatter.ofPattern("yyyy-MM-dd")), LocalDate.now()).getYears
  }

  /**
   *
   * @param systemCodes codes to be matched
   * @param observations list of observations
   * @return The latest observation matching the given codes (Only for observations with no components)
   */
  def findLatestObservation(systemCodes: List[(String, String)], observations: Seq[Observation]): Option[Observation] = {
    observations.filter(obs => checkCodingMatch(obs.code, systemCodes, startsWith = false)).sortBy(obs => obs.effectiveDateTime.getOrElse("0")).reverse.headOption
  }

  /**
   *
   * @param systemCodes codes to be matched
   * @param observations list of observations
   * @return The second latest observation matching the given codes (Only for observations with no components)
   */
  def findSecondLatestObservation(systemCodes: List[(String, String)], observations: Seq[Observation]): Option[Observation] = {
    val list = observations.filter(obs => checkCodingMatch(obs.code, systemCodes, startsWith = false)).sortBy(obs => obs.effectiveDateTime.getOrElse("0")).reverse
    if (list.size < 2) {
      None
    } else {
      Some(list.apply(1))
    }
  }

  /**
   * @param observations list of Observations
   * @return whether all observations have value or not
   */
  def checkObservationValuesExist(observations: List[Option[Observation]]): Boolean = {
    observations.count(opt => {
      opt.isDefined && ((opt.get.valueQuantity.isDefined && opt.get.valueQuantity.get.value.isDefined) || (opt.get.valueBoolean.isDefined) ||
        (opt.get.valueCodeableConcept.isDefined))
    }) == observations.size
  }

  /**
   * @param observation list of Observations
   * @param concept concept ID
   * @return Value of the observation with the given concept ID
   */
  def getQuantityObservationValue(observation: Option[Observation], concept: Option[UnitConceptEnum]): Option[Double] = {
    if (observation.isDefined && observation.get.valueQuantity.isDefined) {
      ObservationUnitUtil.handleUnit(concept.orNull, observation.get.valueQuantity.get, None).value
    } else {
      Option.empty
    }
  }

  def getSystolicBP(BP_SBP: Seq[Observation]) = {
    val sbpObs = FhirParseHelper.findLatestObservation(ValueSetUtil.getConceptSystemAndCode(ConceptIdUtil.SYSTOLIC_BP), BP_SBP)
    val bpObs = FhirParseHelper.findLatestObservation(ValueSetUtil.getConceptSystemAndCode(ConceptIdUtil.BP), BP_SBP)
    if ((sbpObs.isEmpty || !checkObservationValuesExist(List(sbpObs))) && bpObs.nonEmpty) {
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
  }

}
