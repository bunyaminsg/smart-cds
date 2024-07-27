package srdc.smartcds.cds.service

import srdc.smartcds.cds.flow.QRisk3FlowExecution
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import srdc.smartcds.model.fhir.{Condition, FamilyMemberHistory, Goal, MedicationStatement, Observation, Patient}
import org.json4s.{DefaultFormats, JNothing}
import srdc.smartcds.util.{CdsPrefetchUtil, ConceptIdUtil}

import scala.concurrent.{ExecutionContext, Future}

class QRisk3Service(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  /**
   *
   * @param cdsServiceRequest An instance of CdsServiceRequest that enables fetching the patient's inputs
   * @return A Future that represents the result of asynchronous computation of Risk score of patient and a healthy person
   */
  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val fhirPathEvaluator = getFhirPathEvaluator(cdsServiceRequest)
    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      QRisk3FlowExecution.executeFlow(
        age = CdsPrefetchUtil.getAge(fhirPathEvaluator),
        gender = fhirPathEvaluator.evaluateString(CdsPrefetchUtil.GENDER_PATH, JNothing).head,
        atrialFibrillation = CdsPrefetchUtil.existsInt("AtrialFibrillation", fhirPathEvaluator),
        rheumatoidArthritis = CdsPrefetchUtil.existsInt("RheumatoidArthritis", fhirPathEvaluator),
        ckd345 = CdsPrefetchUtil.existsInt("CKD3_4_5", fhirPathEvaluator),
        type1Diabetes = CdsPrefetchUtil.existsInt("Type1Diabetes", fhirPathEvaluator),
        type2Diabetes = CdsPrefetchUtil.existsInt("Type2Diabetes", fhirPathEvaluator),
        hypertensiveTreatment = CdsPrefetchUtil.existsInt("HypertensiveTreatment", fhirPathEvaluator),
        bmi = CdsPrefetchUtil.getObservationValue("BMI", fhirPathEvaluator),
        totalCholesterol = CdsPrefetchUtil.getObservationValue("TotalCholesterol", fhirPathEvaluator),
        hdl = CdsPrefetchUtil.getObservationValue("HDL", fhirPathEvaluator),
        bp = CdsPrefetchUtil.getObservationOrComponentSeqValue("BP_SBP", ConceptIdUtil.SYSTOLIC_BP, fhirPathEvaluator),
        smokingStatus = CdsPrefetchUtil.getSmokingCategory("SmokingStatus", fhirPathEvaluator),
        cvdFmh = CdsPrefetchUtil.existsInt("CVD_FMH", fhirPathEvaluator),
        cvd = CdsPrefetchUtil.exists("CVD", fhirPathEvaluator),
        atorvastatin = CdsPrefetchUtil.exists("Atorvastatin", fhirPathEvaluator),
        systemicLupusErythematosus = CdsPrefetchUtil.existsInt("SystemicLupusErythematosus", fhirPathEvaluator),
        severeMentalIllness = CdsPrefetchUtil.existsInt("SevereMentalIllness", fhirPathEvaluator),
        antipsychotics = CdsPrefetchUtil.existsInt("Antipsychotics", fhirPathEvaluator),
        erectileDysfunction = CdsPrefetchUtil.existsInt("ErectileDysfunction", fhirPathEvaluator),
        migraine = CdsPrefetchUtil.existsInt("Migraine", fhirPathEvaluator),
        corticoSteroids = CdsPrefetchUtil.existsInt("Corticosteroids", fhirPathEvaluator),
        ethnicity = CdsPrefetchUtil.getEthnicityCategory("Ethnicity", fhirPathEvaluator),
        responseBuilder
      ).cdsResponse
    }
  }
}
