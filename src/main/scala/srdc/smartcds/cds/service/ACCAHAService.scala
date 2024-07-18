package cds.service

import cds.flow.ACCAHAFlowExecution
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import model.fhir.{Condition, MedicationStatement, Observation, Patient}
import org.json4s.DefaultFormats

import scala.concurrent.{ExecutionContext, Future}

class ACCAHAService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val Diabetes = cdsServiceRequest.prefetches.get("Diabetes").head.map(res => res.extract[Condition])
    val HypertensiveTreatment = cdsServiceRequest.prefetches.get("HypertensiveTreatment").head.map(res => res.extract[MedicationStatement])
    val Ethnicity = cdsServiceRequest.prefetches.get("Ethnicity").head.map(res => res.extract[Observation])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      ACCAHAFlowExecution.executeFlow(patient, TotalCholesterol, HDL, BP_SBP, SmokingStatus, Diabetes, HypertensiveTreatment, Ethnicity, responseBuilder).cdsResponse
    }
  }
}
