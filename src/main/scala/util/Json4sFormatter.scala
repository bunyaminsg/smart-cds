package util

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.language.implicitConversions

trait JsonClass

object Json4sFormatter {

  implicit lazy val formats: Formats = Serialization.formats(NoTypeHints)

  /**
   * Scala class that adds "parseJson" & "extract" methods to Strings
   */
  class JsonParsable(json: String) {
    def parseJson: JValue = {
      parse(json)
    }

    def extract[T: Manifest]: T = {
      parseJson.extract[T]
    }
  }

  /**
   * Scala class that adds "toJson" method for JObjects
   */
  class JsonConvertable(resource: JsonClass) {
    def toJson: String = {
      Serialization.write(resource)
    }

    def toPrettyJson: String = {
      Serialization.writePretty(resource)
    }
  }

  /**
   * Implicit conversion that ties the new JsonParsable class to the Scala Strings
   */
  implicit def parseFromJson(string: String): JsonParsable = new JsonParsable(string)

  /**
   * Implicit conversion that ties the new JsonConvertable class to JObjects
   */
  implicit def convertToJson(resource: JsonClass): JsonConvertable = new JsonConvertable(resource)
}
