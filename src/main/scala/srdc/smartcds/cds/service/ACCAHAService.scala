package srdc.smartcds.cds.service

import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import org.json4s.{DefaultFormats, JNothing}
import srdc.smartcds.cds.flow.ACCAHAFlowExecution
import srdc.smartcds.util.{CdsPrefetchUtil, ConceptIdUtil}

import scala.concurrent.{ExecutionContext, Future}

class ACCAHAService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val fhirPathEvaluator = getFhirPathEvaluator(cdsServiceRequest)
    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      ACCAHAFlowExecution.executeFlow(
        age = CdsPrefetchUtil.getAge(fhirPathEvaluator),
        gender = fhirPathEvaluator.evaluateString(CdsPrefetchUtil.GENDER_PATH, JNothing).head,
        TotalCholesterol = CdsPrefetchUtil.getObservationValue("TotalCholesterol", fhirPathEvaluator),
        HDLCholesterol = CdsPrefetchUtil.getObservationValue("HDL", fhirPathEvaluator),
        SystolicBP = CdsPrefetchUtil.getObservationOrComponentValue("BP_SBP", ConceptIdUtil.SYSTOLIC_BP, fhirPathEvaluator),
        SmokingStatus = CdsPrefetchUtil.getSmokingCategory("SmokingStatus", fhirPathEvaluator),
        Type1Diabetes = CdsPrefetchUtil.existsInt("Type1Diabetes", fhirPathEvaluator),
        Type2Diabetes = CdsPrefetchUtil.existsInt("Type2Diabetes", fhirPathEvaluator),
        HypertensiveTreatment = CdsPrefetchUtil.existsInt("HypertensiveTreatment", fhirPathEvaluator),
        Ethnicity = CdsPrefetchUtil.getRaceCategory("Ethnicity", fhirPathEvaluator),
        responseBuilder
      ).cdsResponse
    }
  }
}
