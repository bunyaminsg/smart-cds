package model.fhir

case class Meta(profile: Option[Array[String]], versionId: Option[String], lastUpdated: Option[String], tag: Option[Array[Coding]], source: Option[String])
