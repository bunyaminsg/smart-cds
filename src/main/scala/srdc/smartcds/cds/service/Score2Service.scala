package srdc.smartcds.cds.service

import srdc.smartcds.cds.flow.{QRiskFlowExecution, Score2FlowExecution}
import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import srdc.smartcds.model.fhir._
import org.json4s.DefaultFormats

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class Score2Service(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {
    val patient = cdsServiceRequest.prefetches.get("patient").head.head.extract[Patient]
    val TotalCholesterol = cdsServiceRequest.prefetches.get("TotalCholesterol").head.map(res => res.extract[Observation])
    val HDL = cdsServiceRequest.prefetches.get("HDL").head.map(res => res.extract[Observation])
    val BP_SBP = cdsServiceRequest.prefetches.get("BP_SBP").head.map(res => res.extract[Observation])
    val SmokingStatus = cdsServiceRequest.prefetches.get("SmokingStatus").head.map(res => res.extract[Observation])
    val Region = cdsServiceRequest.prefetches.get("Region").head.map(res => res.extract[Observation])

    val responseBuilder = createResponse(cdsServiceRequest)

    Future {
      val score2Result = Score2FlowExecution.executeFlow(patient, TotalCholesterol, HDL, BP_SBP, SmokingStatus, Region, responseBuilder)

      score2Result match {
        case Left(score) => {
          responseBuilder.withCard(_.loadCardWithPostTranslation("card-score",
            "uuid" -> UUID.randomUUID().toString,
            "scoreValue" -> score._1,
            "selectedCell" -> score._2,
            "card" -> score._3
          )).cdsResponse
        }
        case Right(error) => {
          if (error == "missing-input")
            responseBuilder.withCard(_.loadCardWithPostTranslation("card-missing-input",
              "uuid" -> UUID.randomUUID().toString)).cdsResponse
          else if (error == "no-value")
            responseBuilder.withCard(_.loadCardWithPostTranslation("card-no-value",
              "uuid" -> UUID.randomUUID().toString)).cdsResponse
          else responseBuilder.cdsResponse
        }
      }
    }
  }
}
