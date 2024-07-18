package srdc.smartcds.model.fhir

import srdc.smartcds.config.SmartCdsConfig
import srdc.smartcds.util.EpisodeType.EpisodeType
import srdc.smartcds.util.FHIRStatus.FHIRStatus
import srdc.smartcds.util.{EpisodeType, JsonClass}

import scala.util.Try

final case class EpisodeOfCare(resourceType: String,
                               id: String = "",
                               var status: FHIRStatus,
                               var extension: Option[Seq[Extension]],
                               var statusHistory: Option[Seq[EpisodeOfCareStatusHistory]],
                               var `type`: Option[Seq[CodeableConcept]],
                               var period: Option[Period],
                               patient: Reference,
                               identifier: Option[Seq[Identifier]] = None
                              ) extends JsonClass {

  def withStatus(status: FHIRStatus): EpisodeOfCare = {
    this.copy(status = status)
  }

  def withStatusHistory(statusHistory: Option[Seq[EpisodeOfCareStatusHistory]]): EpisodeOfCare = {
    this.copy(statusHistory = statusHistory)
  }

  def withExtension(extension: Option[Seq[Extension]]): EpisodeOfCare = {
    this.copy(extension = extension)
  }

  def getType: EpisodeType = {
    Try(`type`.get.head.coding.head.code).getOrElse(EpisodeType.SCREENING)
  }

  def getStatusToBeSet: Option[FHIRStatus] = {
    Try(extension.get.find(_.url == SmartCdsConfig.STATUS_TO_BE_SET).get.valueString.get).toOption
  }

  def getEpisodeTypeToContinue: EpisodeType = {
    Try(extension.get.find(_.url == SmartCdsConfig.EPISODE_TYPE_TO_CONTINUE).get.valueString.get).getOrElse(getType)
  }

  def getPeriodStart: String = {
    Try(period.get.start.get).getOrElse("")
  }

}

final case class EpisodeOfCareStatusHistory(extension: Seq[Extension],
                                            status: FHIRStatus,
                                            period: Period) extends JsonClass



