package model.fhir

case class AnswerOption(valueCoding: Coding, initialSelected: Option[Boolean])
