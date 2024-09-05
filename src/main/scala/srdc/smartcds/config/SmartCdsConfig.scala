package srdc.smartcds.config

import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Try

object SmartCdsConfig {
  /** Application config object. */
  val config = ConfigFactory.load()

  /** Rule Units */
  lazy val ruleUnits: Config = config.getConfig("onfhir.cds.rule-units")

  /** Definitions Path */
  lazy val definitionsPath: Option[String] = Try(config.getString("onfhir.cds.definitions-path")).toOption
  lazy val conceptDefinitionsPath: Option[String] = Try(config.getString("app.concept-definitions-path")).toOption

  /** Bundles Path */
  lazy val bundlesPath: Option[String] = Try(config.getString("app.kroniq-bundles-path")).toOption
  lazy val valueSetsPath: Option[String] = Try(config.getString("app.kroniq-valuesets-path")).toOption

  // EpisodeOfCare.extension urls
  final val STATUS_TO_BE_SET: String = "http://kroniq.srdc.com.tr/fhir/StructureDefinition/status-to-be-set"
  final val EPISODE_TYPE_TO_CONTINUE: String = "http://kroniq.srdc.com.tr/fhir/StructureDefinition/episode-type-to-continue"
  final val PAGE_TO_CONTINUE: String = "http://kroniq.srdc.com.tr/fhir/StructureDefinition/page-to-continue"
  final val WIZARD_PATH: String = "http://kroniq.srdc.com.tr/fhir/StructureDefinition/wizard-path"

  final val WIZARD_PATH_SEPARATOR = "\\|"

}
