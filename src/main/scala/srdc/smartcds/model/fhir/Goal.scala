package srdc.smartcds.model.fhir

case class Goal(
               id: String,
               lifecycleStatus: String,
               achievementStatus: Option[CodeableConcept],
               subject: Reference,
               target: Option[Array[GoalTarget]],
               description: CodeableConcept
               )
