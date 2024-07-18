package srdc.smartcds.model.fhir

case class ContactPoint(system: Option[String], value: Option[String], use: Option[String], rank: Option[Int], period: Option[Period])
