package srdc.smartcds.cds.service

import io.onfhir.cds.api.model.CdsResponse
import io.onfhir.cds.service.{BaseCdsService, CdsServiceContext, CdsServiceRequest}
import io.onfhir.util.JsonFormatter.convertToJson2
import org.json4s.DefaultFormats
import srdc.smartcds.util.DefinitionUtil

import scala.concurrent.{ExecutionContext, Future}

class DefinitionService(cdsServiceContext: CdsServiceContext) extends BaseCdsService(cdsServiceContext) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  /**
   * @param cdsServiceRequest Request should include the ID of the requested service
   * @param ex
   * @return CDS Response with the definitions of clinical concepts used in the given CDS service
   */
  override def executeCds(cdsServiceRequest: CdsServiceRequest)(implicit ex: ExecutionContext): Future[CdsResponse] = {

    val responseBuilder = createResponse(cdsServiceRequest)

    val serviceId = cdsServiceRequest.contextParams("serviceId").extract[String]
    val definition = DefinitionUtil.getDefinitions(serviceId, cdsServiceRequest.preferredLang)

    Future {
      responseBuilder.withCard(_.loadCardWithPostTranslation("card-info",
        "serviceId" -> serviceId,
        "detail" -> definition.toJson
      )).cdsResponse
    }

  }
}
