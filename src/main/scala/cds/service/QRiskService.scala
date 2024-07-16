package cds.service

import cds.flow.QRiskFlowExecution
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import model.fhir.{Condition, FamilyMemberHistory, Goal, MedicationStatement, Observation, Patient}
import org.json4s.DefaultFormats

import scala.concurrent.{ExecutionContext, Future}

class QRiskService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val AtrialFibrillation = cdsServiceRequest.prefetches.get("AtrialFibrillation").head.map(res => res.extract[Condition])
    val RheumatoidArthritis = cdsServiceRequest.prefetches.get("RheumatoidArthritis").head.map(res => res.extract[Condition])
    val CKD4_5 = cdsServiceRequest.prefetches.get("CKD4_5").head.map(res => res.extract[Condition])
    val Type1Diabetes = cdsServiceRequest.prefetches.get("Type1Diabetes").head.map(res => res.extract[Condition])
    val Type2Diabetes = cdsServiceRequest.prefetches.get("Type2Diabetes").head.map(res => res.extract[Condition])
    val HypertensiveTreatment = cdsServiceRequest.prefetches.get("HypertensiveTreatment").head.map(res => res.extract[MedicationStatement])
    val BMI = cdsServiceRequest.prefetches.get("BMI").head.map(res => res.extract[Observation])
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val CVD_FMH = cdsServiceRequest.prefetches.get("CVD_FMH").head.map(res => res.extract[FamilyMemberHistory])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      QRiskFlowExecution.executeFlow(patient, AtrialFibrillation, RheumatoidArthritis, CKD4_5, Type1Diabetes, Type2Diabetes,
        HypertensiveTreatment, BMI, TotalCholesterol, HDL, BP_SBP, SmokingStatus, CVD_FMH, responseBuilder).cdsResponse
    }
  }
}
