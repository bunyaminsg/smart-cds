import cds.CdsServiceFactory
import io.onfhir.cds.OnFhirCds

object Boot extends App {
  val onfhirCds = OnFhirCds.asStandaloneServer(CdsServiceFactory)
  onfhirCds.start()
}