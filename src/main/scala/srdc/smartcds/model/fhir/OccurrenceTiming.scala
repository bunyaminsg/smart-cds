package srdc.smartcds.model.fhir

case class OccurrenceTiming(repeat: Repeat)

case class Repeat(boundsPeriod: BoundsPeriod, frequency: Option[Int], period: Option[Int], periodUnit: Option[String],
                  when: Option[List[String]], dayOfWeek: Option[List[String]], timeOfDay: Option[List[String]])

case class BoundsPeriod(start: Option[String], var end: Option[String])
