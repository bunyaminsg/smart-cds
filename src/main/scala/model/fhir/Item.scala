package model.fhir

case class Item(linkId: String,
                prefix: Option[String],
                text: Option[String],
                `type`: Option[String],
                required: Option[Boolean],
                item: Option[Array[Item]],
                answerOption: Option[Array[AnswerOption]],
                answer: Option[Array[Answer]],
                code: Option[Array[Coding]])
