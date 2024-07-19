package srdc.smartcds.cds.service

import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import org.json4s.DefaultFormats
import srdc.smartcds.cds.flow.AdvanceFlowExecution
import srdc.smartcds.model.fhir.{Condition, MedicationStatement, Observation, Patient}

import scala.concurrent.{ExecutionContext, Future}

class AdvanceService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val AtrialFibrillation = cdsServiceRequest.prefetches.get("AtrialFibrillation").head.map(res => res.extract[Condition])
    val DiabeticRetinopathy = cdsServiceRequest.prefetches.get("DiabeticRetinopathy").head.map(res => res.extract[Observation])
    val HypertensiveTreatment = cdsServiceRequest.prefetches.get("HypertensiveTreatment").head.map(res => res.extract[MedicationStatement])
    val HbA1C = cdsServiceRequest.prefetches.get("HbA1C").head.map(res => res.extract[Observation])
    val ACR = cdsServiceRequest.prefetches.get("ACR").head.map(res => res.extract[Observation])
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val NonHDL = cdsServiceRequest.prefetches.get("NonHDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val BP_DBP = cdsServiceRequest.prefetches.get("BP_DBP").head.map(res => res.extract[Observation])
    val Type1Diabetes = cdsServiceRequest.prefetches.get("Type1Diabetes").head.map(res => res.extract[Condition])
    val Type2Diabetes = cdsServiceRequest.prefetches.get("Type2Diabetes").head.map(res => res.extract[Condition])

    val responseBuilder = createResponse(cdsServiceRequest)
    Future{
      AdvanceFlowExecution.executionFlow(patient, AtrialFibrillation, DiabeticRetinopathy, HypertensiveTreatment,
        HbA1C, ACR, TotalCholesterol, HDL, NonHDL, BP_SBP, BP_DBP, Type1Diabetes, Type2Diabetes,  responseBuilder).cdsResponse
    }
  }
}
