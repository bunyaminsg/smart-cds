package srdc.smartcds.model.fhir

case class Person(resourceType: String, id: Option[String], active: Option[Boolean], birthDate: Option[String], extension: Option[Array[Extension]], link: Option[Array[PersonLink]],
                  gender: Option[String], address: Option[Array[Address]], name: Option[Array[HumanName]], identifier: Option[Array[Identifier]], telecom: Option[Array[ContactPoint]])

case class PersonLink(target: Reference)

case class RelatedPerson(resourceType: String, id: Option[String], active: Option[Boolean], birthDate: Option[String],
                         patient: Reference, relationship: Option[Array[CodeableConcept]], extension: Option[Array[Extension]], gender: Option[String],
                         address: Option[Array[Address]], name: Option[Array[HumanName]], identifier: Option[Array[Identifier]], telecom: Option[Array[ContactPoint]])

object RelatedPerson {
  def combined(person: Person, relatedPerson: RelatedPerson): RelatedPerson = {
    val rpExtensions = relatedPerson.extension.getOrElse(Array.empty)
    val pExtensions = person.extension.getOrElse(Array.empty)
    RelatedPerson(relatedPerson.resourceType, relatedPerson.id, relatedPerson.active, person.birthDate,
      relatedPerson.patient, relatedPerson.relationship,
      Some(rpExtensions.filter(ext => !pExtensions.exists(_.url == ext.url)) ++ pExtensions),
      person.gender, person.address, person.name, person.identifier, person.telecom
    )
  }
}