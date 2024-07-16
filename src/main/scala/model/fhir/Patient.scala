package model.fhir

case class Patient(resourceType: String, id: String, active: Option[Boolean], birthDate: Option[String], gender: Option[String], address: Option[Array[Address]], name: Option[Array[HumanName]], identifier: Option[Array[Identifier]], telecom: Option[Array[ContactPoint]])
case class HumanName(family: Option[String], given: Option[Array[String]])
case class Identifier(system: Option[String], value: Option[String])
