package srdc.smartcds.cds.service

import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import org.json4s.{DefaultFormats, JNothing}
import srdc.smartcds.cds.flow.AdvanceFlowExecution
import srdc.smartcds.util.{CdsPrefetchUtil, ConceptIdUtil}

import scala.concurrent.{ExecutionContext, Future}

class AdvanceService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val fhirPathEvaluator = getFhirPathEvaluator(cdsServiceRequest)
    val responseBuilder = createResponse(cdsServiceRequest)

    Future{
      AdvanceFlowExecution.executionFlow(
        age = CdsPrefetchUtil.getAge(fhirPathEvaluator),
        gender = fhirPathEvaluator.evaluateString(CdsPrefetchUtil.GENDER_PATH, JNothing).head,
        atrialFibrillation = CdsPrefetchUtil.exists("AtrialFibrillation", fhirPathEvaluator),
        retinopathy = CdsPrefetchUtil.exists("Retinopathy", fhirPathEvaluator),
        hypertensiveTreatment = CdsPrefetchUtil.exists("HypertensiveTreatment", fhirPathEvaluator),
        hba1cOpt = CdsPrefetchUtil.getObservationValue("HbA1C", fhirPathEvaluator),
        acrOpt = CdsPrefetchUtil.getObservationValue("ACR", fhirPathEvaluator),
        totalCholesterolOpt = CdsPrefetchUtil.getObservationValue("TotalCholesterol", fhirPathEvaluator),
        hdlOpt = CdsPrefetchUtil.getObservationValue("HDL", fhirPathEvaluator),
        sbpOpt = CdsPrefetchUtil.getObservationOrComponentValue("BP_SBP", ConceptIdUtil.SYSTOLIC_BP, fhirPathEvaluator),
        dbpOpt = CdsPrefetchUtil.getObservationOrComponentValue("BP_DBP", ConceptIdUtil.DIASTOLIC_BP, fhirPathEvaluator),
        type1Diabetes = CdsPrefetchUtil.exists("Type1Diabetes", fhirPathEvaluator),
        type2Diabetes = CdsPrefetchUtil.exists("Type2Diabetes", fhirPathEvaluator),
        type1DiabetesDuration = CdsPrefetchUtil.getConditionDuration("Type1Diabetes", fhirPathEvaluator),
        type2DiabetesDuration = CdsPrefetchUtil.getConditionDuration("Type2Diabetes", fhirPathEvaluator),
        ckd45 = CdsPrefetchUtil.exists("CKD4_5", fhirPathEvaluator),
        cvd = CdsPrefetchUtil.exists("CVD", fhirPathEvaluator),
        atorvastatin = CdsPrefetchUtil.exists("Atorvastatin", fhirPathEvaluator),
        responseBuilder
      ).cdsResponse
    }
  }
}
