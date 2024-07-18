package srdc.smartcds.model.fhir

import srdc.smartcds.util.JsonClass

case class FamilyMemberHistory (condition: Option[Seq[FamilyMemberHistoryCondition]])

final case class FamilyMemberHistoryCondition(code: CodeableConcept) extends JsonClass