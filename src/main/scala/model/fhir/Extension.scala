package model.fhir

import util.JsonClass

final case class Extension(url: String,
                           valueString: Option[String] = None,
                           valueBoolean: Option[Boolean] = None,
                           valueCode: Option[String] = None) extends JsonClass {

  def withValueString(value: String): Extension = {
    this.copy(valueString = Some(value))
  }

  def withValueBoolean(value: Boolean): Extension = {
    this.copy(valueBoolean = Some(value))
  }
}
