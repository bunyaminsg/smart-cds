package srdc.smartcds.cds.service

import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import org.json4s.DefaultFormats
import srdc.smartcds.cds.flow.ACCAHAFlowExecution
import srdc.smartcds.model.fhir.{Condition, MedicationStatement, Observation, Patient}

import scala.concurrent.{ExecutionContext, Future}

class ACCAHAService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val Type1Diabetes = cdsServiceRequest.prefetches.get("Type1Diabetes").head.map(res => res.extract[Condition])
    val Type2Diabetes = cdsServiceRequest.prefetches.get("Type2Diabetes").head.map(res => res.extract[Condition])
    val HypertensiveTreatment = cdsServiceRequest.prefetches.get("HypertensiveTreatment").head.map(res => res.extract[MedicationStatement])
    val Ethnicity = cdsServiceRequest.prefetches.get("Ethnicity").head.map(res => res.extract[Observation])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      ACCAHAFlowExecution.executeFlow(patient, TotalCholesterol, HDL, BP_SBP, SmokingStatus, Type1Diabetes, Type2Diabetes,
        HypertensiveTreatment, Ethnicity, responseBuilder).cdsResponse
    }
  }
}
