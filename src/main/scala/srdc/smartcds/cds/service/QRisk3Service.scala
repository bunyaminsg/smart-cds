package srdc.smartcds.cds.service

import srdc.smartcds.cds.flow.QRisk3FlowExecution
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import srdc.smartcds.model.fhir.{Condition, FamilyMemberHistory, Goal, MedicationStatement, Observation, Patient}
import org.json4s.DefaultFormats

import scala.concurrent.{ExecutionContext, Future}

class QRisk3Service(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val AtrialFibrillation = cdsServiceRequest.prefetches.get("AtrialFibrillation").head.map(res => res.extract[Condition])
    val RheumatoidArthritis = cdsServiceRequest.prefetches.get("RheumatoidArthritis").head.map(res => res.extract[Condition])
    val CKD3_4_5 = cdsServiceRequest.prefetches.get("CKD3_4_5").head.map(res => res.extract[Condition])
    val Type1Diabetes = cdsServiceRequest.prefetches.get("Type1Diabetes").head.map(res => res.extract[Condition])
    val Type2Diabetes = cdsServiceRequest.prefetches.get("Type2Diabetes").head.map(res => res.extract[Condition])
    val HypertensiveTreatment = cdsServiceRequest.prefetches.get("HypertensiveTreatment").head.map(res => res.extract[MedicationStatement])
    val BMI = cdsServiceRequest.prefetches.get("BMI").head.map(res => res.extract[Observation])
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val CVD_FMH = cdsServiceRequest.prefetches.get("CVD_FMH").head.map(res => res.extract[FamilyMemberHistory])
    val SystemicLupusErythematosus = cdsServiceRequest.prefetches.get("SystemicLupusErythematosus").head.map(res => res.extract[Condition])
    val SevereMentalIllness = cdsServiceRequest.prefetches.get("SevereMentalIllness").head.map(res => res.extract[Condition])
    val Antipsychotics = cdsServiceRequest.prefetches.get("Antipsychotics").head.map(res => res.extract[MedicationStatement])
    val ErectileDysfunction = cdsServiceRequest.prefetches.get("ErectileDysfunction").head.map(res => res.extract[Condition])
    val Migraine = cdsServiceRequest.prefetches.get("Migraine").head.map(res => res.extract[Condition])
    val Corticosteroids = cdsServiceRequest.prefetches.get("Corticosteroids").head.map(res => res.extract[MedicationStatement])
    val Ethnicity = cdsServiceRequest.prefetches.get("Ethnicity").head.map(res => res.extract[Observation])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      QRisk3FlowExecution.executeFlow(patient, AtrialFibrillation, RheumatoidArthritis, CKD3_4_5, Type1Diabetes, Type2Diabetes,
        HypertensiveTreatment, BMI, TotalCholesterol, HDL, BP_SBP, SmokingStatus, CVD_FMH, Corticosteroids, Antipsychotics,
        Ethnicity, SystemicLupusErythematosus, SevereMentalIllness, ErectileDysfunction, Migraine, responseBuilder).cdsResponse
    }
  }
}
