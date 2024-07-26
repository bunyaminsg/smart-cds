package srdc.smartcds.util

object FHIRResourceTypes extends Enumeration {
  type FHIRResourceTypes = String
  val ALLERGY_INTOLERANCE = "AllergyIntolerance"
  val APPOINTMENT = "Appointment"
  val BASIC = "Basic"
  val CAREPLAN = "CarePlan"
  val CARETEAM = "CareTeam"
  val COMMUNICATION = "Communication"
  val COMMUNICATION_REQUEST = "CommunicationRequest"
  val CONDITION = "Condition"
  val EPISODE_OF_CARE = "EpisodeOfCare"
  val GOAL = "Goal"
  val MEDICATION = "Medication"
  val MEDICATION_REQUEST = "MedicationRequest"
  val MEDICATION_STATEMENT = "MedicationStatement"
  val OBSERVATION = "Observation"
  val PATIENT = "Patient"
  val PARAMETERS = "Parameters"
  val PROCEDURE = "Procedure"
  val QUESTIONNAIRE_RESPONSE = "QuestionnaireResponse"
  val VALUE_SET = "ValueSet"
  val RELATED_PERSON = "RelatedPerson"
  val PERSON = "Person"
  val PRACTITIONER = "Practitioner"
  val SERVICE_REQUEST = "ServiceRequest"
}

object FHIRStatus extends Enumeration {
  type FHIRStatus = String
  val ACTIVE = "active"
  val DRAFT = "draft"
  val PLANNED = "planned"
  val WAITLIST = "waitlist"
  val ONHOLD = "onhold"
  val FINISHED = "finished"
  val REVOKED = "revoked"
  val COMPLETED = "completed"
  val CANCELLED = "cancelled"
  val ENTERED_IN_ERROR = "entered-in-error"
  val UNKNOWN = "unknown"
}

object EventStatus extends Enumeration {
  type EventStatus = String
  val PREPARATION = "preparation"
  val IN_PROGRESS = "in-progress"
  val NOT_DONE = "not-done"
  val ON_HOLD = "on-hold"
  val STOPPED = "stopped"
  val COMPLETED = "completed"
  val ENTERED_IN_ERROR = "entered-in-error"
  val UNKNOWN = "unknown"
}

object CareTeamStatus extends Enumeration {
  type CareTeamStatus = String
  val PROPOSED = "proposed"
  val ACTIVE = "active"
  val SUSPENDED = "suspended"
  val INACTIVE = "inactive"
  val ENTERED_IN_ERROR = "entered-in-error"
}

object CarePlanIntent extends Enumeration {
  type CarePlanIntent = String
  val PROPOSAL = "proposal"
  val PLAN = "plan"
  val ORDER = "order"
  val OPTION = "option"
}

object EpisodeType extends Enumeration {
  type EpisodeType = String
  val SCREENING = "screening"
  val MONITORING = "monitoring"
}

object UnitConceptEnum extends Enumeration {
  type UnitConceptEnum = String
  val ACR = "acr"
  val CHOLESTEROL = "cholesterol"
  val FASTING_GLUCOSE = "fasting-glucose"
  val HBA1C = "hba1c"
  val SERUM_POTASSIUM = "serum-potassium"
  val SERUM_CREATININE = "serum-creatinine"
  val URINE_CREATININE = "urine-creatinine"
  val TRIGLYCERIDES = "triglycerides"
}

object ObservationUnitEnum extends Enumeration {
  type ObservationUnitEnum = String
  val MMOL_L = "mmol/l"
  val MG_DL = "mg/dl"
  val MMOL_MOL = "mmol/mol"
  val PERCENT = "%"
  val MG_G = "mg/g"
  val MG_MMOL = "mg/mmol"
  val MG_MG = "mg/mg"
  val UG_UMOL = "µg/µmol"
  val UMOL_L = "µmol/l"
  val MEQ_L = "meq/l"
}

object SmokingCategoryEnum extends Enumeration {
  type SmokingCategoryEnum = Int
  val NEVER = 0
  val FORMER = 1
  val LIGHT = 2
  val DAILY = 3
  val HEAVY = 4
}