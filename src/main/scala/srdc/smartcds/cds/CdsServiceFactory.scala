package srdc.smartcds.cds

import srdc.smartcds.cds.service.ACCAHAService
import srdc.smartcds.cds.service.{DefinitionService, QRiskService, Score2Service, SmartRiskService}
import io.onfhir.cds.service.{CdsServiceContext, ICdsService, ICdsServiceFactory}

object CdsServiceFactory extends ICdsServiceFactory {

  /**
   * Service IDs implemented in the project
   */
  val servicesSupported: Set[String] = Set("qrisk", "definition", "score2", "smart", "acc_aha")

  /**
   * Checks if there is an implemented service with the given service ID
   * @param serviceId CDS Service ID
   * @return
   */
  override def isServiceSupported(serviceId: String): Boolean = {
    servicesSupported.contains(serviceId)
  }

  /**
   * Creates Service instant with the given context and service ID
   * @param cdsServiceContext Context of the CDS request to be passed on the services
   * @return
   */
  override def createCdsService(cdsServiceContext: CdsServiceContext): ICdsService = {
    cdsServiceContext.serviceId match {
      case "qrisk" => new QRiskService(cdsServiceContext)
      case "definition" => new DefinitionService(cdsServiceContext)
      case "score2" => new Score2Service(cdsServiceContext)
      case "smart" => new SmartRiskService(cdsServiceContext)
      case "acc_aha" => new ACCAHAService(cdsServiceContext)
      case other => throw new Exception(s"Service $other not supported!")
    }
  }
}
