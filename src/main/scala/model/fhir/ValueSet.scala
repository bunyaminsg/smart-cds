package model.fhir

case class ValueSet(id: Option[String], compose: Compose)

case class Compose(include: Option[Seq[Concept]])

case class Concept(system: Option[String], concept: Option[Seq[Coding]])