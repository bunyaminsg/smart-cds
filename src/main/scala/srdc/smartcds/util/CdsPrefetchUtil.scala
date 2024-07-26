package srdc.smartcds.util

import io.onfhir.path.FhirPathEvaluator
import org.json4s.JNothing

import java.time.{LocalDate, Period}

/**
 * Helper utilities to obtain required values from the CDS prefetch
 */
object CdsPrefetchUtil {
  /** FHIR path of patient's birth date */
  final val DATE_OF_BIRTH_PATH = "%cdsPrefetch.patient.birthDate"
  /** FHIR path of patient's gender */
  final val GENDER_PATH = "%cdsPrefetch.patient.gender"

  /** FHIR Paths for specific fields */
  private def observationValuePath(prefetchKey: String) = s"%cdsPrefetch.$prefetchKey.valueQuantity.value"
  private def existsPath(prefetchKey: String) = s"%cdsPrefetch.$prefetchKey.exists()"
  private def observationOrComponentValuePath(prefetchKey: String, conceptId: String): String = {
    val codes = ValueSetUtil.getConceptSystemAndCode(conceptId)
    val codesStr = codes.map(coding => s"'${coding._1}|${coding._2}'").mkString(",")
    s"%cdsPrefetch.$prefetchKey.where($$this.code.coding.isCodingIn($codesStr)).valueQuantity.value || " +
      s"%cdsPrefetch.$prefetchKey.component.where($$this.code.coding.isCodingIn($codesStr)).valueQuantity.value"
  }

  /**
   * Checks if any resource for the given prefetch exists
   * @param prefetchKey
   * @param fhirPathEvaluator
   * @return Boolean
   */
  def exists(prefetchKey: String, fhirPathEvaluator: FhirPathEvaluator): Boolean = {
    fhirPathEvaluator.satisfies(CdsPrefetchUtil.existsPath(prefetchKey), JNothing)
  }

  /**
   * Checks if any resource for the given prefetch exists and returns 1 or 0
   * @param prefetchKey
   * @param fhirPathEvaluator
   * @return Int
   */
  def existsInt(prefetchKey: String, fhirPathEvaluator: FhirPathEvaluator): Int = {
    if (exists(prefetchKey, fhirPathEvaluator)) 1 else 0
  }

  /**
   * Gets valueQuantity.value for given prefetch
   * @param prefetchKey
   * @param fhirPathEvaluator
   * @return
   */
  def getObservationValue(prefetchKey: String, fhirPathEvaluator: FhirPathEvaluator): Option[Double] = {
    fhirPathEvaluator.evaluateOptionalNumerical(CdsPrefetchUtil.observationValuePath(prefetchKey), JNothing).map(_.toDouble)
  }

  /**
   * Gets valueQuantity.value or component.valueQuantity.value for given concept in the given prefetch
   * @param prefetchKey Prefetch key
   * @param conceptId Concept ID to lookup the codes
   * @param fhirPathEvaluator
   * @return
   */
  def getObservationOrComponentValue(prefetchKey: String, conceptId: String, fhirPathEvaluator: FhirPathEvaluator): Option[Double] = {
    fhirPathEvaluator.evaluateOptionalNumerical(CdsPrefetchUtil.observationOrComponentValuePath(prefetchKey, conceptId), JNothing).map(_.toDouble)
  }

  /**
   * Determines the smoking category using given prefetch
   * @param prefetchKey
   * @param fhirPathEvaluator
   * @return Smoking Category in scale of 0 to 4 (0-Never Smoker, 1-Former Smoker, 2-Light Smoker, 3-Every day smoker, 4-Heavy Smoker)
   */
  def getSmokingCategory(prefetchKey: String, fhirPathEvaluator: FhirPathEvaluator): Option[Int] = {
    fhirPathEvaluator.evaluateOptionalString(s"%cdsPrefetch.$prefetchKey.valueCodeableConcept.coding.code", JNothing) match {
      case Some("LA18978-9"|"LA18980-5"|"266919005") => Some(SmokingCategoryEnum.NEVER)
      case Some("LA15920-4"|"8517006") => Some(SmokingCategoryEnum.FORMER)
      case Some("LA18977-1"|"LA18982-1") => Some(SmokingCategoryEnum.LIGHT)
      case Some("LA18979-7"|"LA18976-3"|"449868002") => Some(SmokingCategoryEnum.DAILY)
      case Some("LA18981-3") => Some(SmokingCategoryEnum.HEAVY)
      case _ => None
    }
  }

  /**
   * Calculates the age of the patient
   * @param fhirPathEvaluator
   * @return
   */
  def getAge(fhirPathEvaluator: FhirPathEvaluator): Int = {
    val dob = fhirPathEvaluator.evaluateDateTime(CdsPrefetchUtil.DATE_OF_BIRTH_PATH, JNothing).head.asInstanceOf[LocalDate]
    Period.between(dob, LocalDate.now()).getYears
  }

}
