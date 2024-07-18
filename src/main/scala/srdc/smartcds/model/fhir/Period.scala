package srdc.smartcds.model.fhir

import srdc.smartcds.util.JsonClass

final case class Period(start: Option[String],
                        end: Option[String]) extends JsonClass
