package srdc.smartcds.model.fhir

import srdc.smartcds.util.JsonClass

case class Parameters(resourceType: String,
                      var parameter: Seq[Parameter]) extends JsonClass

case class Parameter(name: String,
                     valueString: Any) extends JsonClass
