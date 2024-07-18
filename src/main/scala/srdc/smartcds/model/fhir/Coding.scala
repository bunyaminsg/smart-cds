package srdc.smartcds.model.fhir

case class Coding(var code: String, display: Option[String], system: Option[String], designation: Option[Seq[Designation]] = None)

case class Designation(language: Option[String], value: Option[String])
