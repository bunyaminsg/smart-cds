package srdc.smartcds.model.fhir

case class QuestionnaireResponse(resourceType: String,
                                 identifier: Option[Identifier],
                                 meta: Option[Meta],
                                 contained: Option[Array[Questionnaire]],
                                 questionnaire: String,
                                 status: String,
                                 item: Option[Array[Item]])
