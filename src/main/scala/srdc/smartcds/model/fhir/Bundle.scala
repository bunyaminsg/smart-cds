package srdc.smartcds.model.fhir

import io.onfhir.api.Resource

case class Bundle(entry: List[BundleEntry])

case class BundleEntry(resource: Resource)
