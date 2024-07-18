package srdc.smartcds.model.fhir

case class Questionnaire(resourceType: String, id: String, status: String, meta: Meta, item: Option[Array[Item]], title: Option[String], code: Option[Seq[Coding]])
