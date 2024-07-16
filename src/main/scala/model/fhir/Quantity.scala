package model.fhir

case class Quantity(var value: Option[Double], var unit: Option[String], code: Option[String], system: Option[String], comparator: Option[String])
