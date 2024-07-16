package model.fhir

import util.JsonClass

final case class Period(start: Option[String],
                        end: Option[String]) extends JsonClass
