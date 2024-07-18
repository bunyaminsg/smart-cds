package srdc.smartcds.model.fhir

case class GoalTarget(
                     measure: Option[CodeableConcept],
                     detailRange: Option[TargetRange]
                     )
