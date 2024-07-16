package model.fhir

case class Annotation(authorReference: Option[Reference],
                      time: Option[String],
                      text: String,
                      extension: Option[Seq[Extension]])
