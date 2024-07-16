package model.fhir

import util.JsonClass

case class FamilyMemberHistory (condition: Option[Seq[FamilyMemberHistoryCondition]])

final case class FamilyMemberHistoryCondition(code: CodeableConcept) extends JsonClass