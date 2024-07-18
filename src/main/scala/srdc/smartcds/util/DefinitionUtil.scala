package srdc.smartcds.util

import srdc.smartcds.config.SmartCdsConfig
import org.json4s.JsonAST.{JArray, JField, JNothing, JObject, JString}
import srdc.smartcds.util.Json4sFormatter._

import scala.io.Source

object DefinitionUtil {
  /** ServiceID -> Concept Definitions map */
  var conceptDefinitionMap: Map[String, JObject] = Map()

  /** Read the concept definitions from files */
  {
    try {
      println(s"definitions ${SmartCdsConfig.conceptDefinitionsPath.get}")
      val definitions = new java.io.File(SmartCdsConfig.conceptDefinitionsPath.get).listFiles.filter(_.getName.endsWith(".json"))
      println(definitions.length)
      for (definitionPath <- definitions) {
        val source = Source.fromFile(definitionPath)(scala.io.Codec("UTF-8"))
        val definitionObject = source.mkString.parseJson.asInstanceOf[JObject]
        conceptDefinitionMap += ((definitionObject \ "serviceId").extract[String] -> (definitionObject \ "concepts").extract[JObject])
      }
    } catch {
      case e: Exception =>
        println(s"Error while reading definitions", e)
    }
  }

  /**
   * Parses the service definition for the given service, process the translations and selections
   * @param serviceId ID of the CDS service
   * @param preferredLang Preferred Language
   * @return Concept definitions for given service
   */
  def getDefinitions(serviceId: String, preferredLang: Option[String] = None): JObject = {
    try {
      val definitions = conceptDefinitionMap(serviceId)
      definitions.transformField {
        case (key: String, JObject(value)) =>
          val conceptId = value.find(_._1 == "conceptId").map(_._2.extract[String]).getOrElse(key)
          val codes = ValueSetUtil.getConceptSystemCodeAndDisplay(conceptId, preferredLang.getOrElse("en")).map(concept => JObject(List(
            JField("system", JString(concept._1)),
            JField("code", JString(concept._2)),
            JField("display", JString(concept._3))
          )))
          key -> JObject(value.map {
            // keep label if string
            case ("label", JString(labelField)) => ("label", JString(labelField))
            // select label with preferred language if translations are provided
            case ("label", JObject(labelField)) =>
              if (preferredLang.nonEmpty && labelField.exists(el => el._1 == preferredLang.get)) {
                ("label", labelField.find(el => el._1 == preferredLang.get).get._2)
              } else if (labelField.exists(el => el._1 == "en")) {
                ("label", labelField.find(el => el._1 == "en").get._2)
              } else {
                ("label", JNothing)
              }
            // set selection options from Bundle or ValueSet
            case ("select", JString(selectPath)) =>
              if (selectPath.startsWith("Bundle")) {
                ("select", JArray(codes))
              } else if (selectPath.startsWith("ValueSet")) {
                val options = ValueSetUtil.getConceptsFromValueSet(selectPath.split("/").last, preferredLang)
                ("select", JArray(options.toList))
              } else {
                ("select", JNothing)
              }
            case other => other
          } ++ List(JField("code", codes.headOption.getOrElse(JNothing)))).extract[JObject] // add a default code
      }.extract[JObject]
    } catch {
      case e: Exception =>
        println(s"Error while searching definitions: $serviceId", e)
        JObject()
    }
  }
}
