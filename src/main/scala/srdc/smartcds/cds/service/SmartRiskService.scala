package srdc.smartcds.cds.service

import srdc.smartcds.cds.flow.{QRiskFlowExecution, SmartRiskFlowExecution}
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import srdc.smartcds.model.fhir._
import org.json4s.DefaultFormats

import scala.concurrent.{ExecutionContext, Future}

class SmartRiskService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val Type1Diabetes = cdsServiceRequest.prefetches.get("Type1Diabetes").head.map(res => res.extract[Condition])
    val Type2Diabetes = cdsServiceRequest.prefetches.get("Type2Diabetes").head.map(res => res.extract[Condition])
    val CerebrovascularDisease = cdsServiceRequest.prefetches.get("CerebrovascularDisease").head.map(res => res.extract[Condition])
    val AcuteCoronarySyndrome = cdsServiceRequest.prefetches.get("AcuteCoronarySyndrome").head.map(res => res.extract[Condition])
    val ChronicCoronarySyndromes = cdsServiceRequest.prefetches.get("ChronicCoronarySyndromes").head.map(res => res.extract[Condition])
    val AorticAneurysm = cdsServiceRequest.prefetches.get("AorticAneurysm").head.map(res => res.extract[Condition])
    val PeripheralArteryDisease = cdsServiceRequest.prefetches.get("PeripheralArteryDisease").head.map(res => res.extract[Condition])

    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val CRP = cdsServiceRequest.prefetches.get("CRP").head.map(res => res.extract[Observation])
    val Egfr = cdsServiceRequest.prefetches.get("Egfr").head.map(res => res.extract[Observation])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      SmartRiskFlowExecution.executeFlow(patient, Type1Diabetes, Type2Diabetes, CerebrovascularDisease, AcuteCoronarySyndrome,
        ChronicCoronarySyndromes, AorticAneurysm, PeripheralArteryDisease, TotalCholesterol, HDL, BP_SBP, SmokingStatus, CRP, Egfr,
        responseBuilder).cdsResponse
    }
  }
}
