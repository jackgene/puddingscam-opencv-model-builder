package model

import java.awt.Dimension

import play.api.libs.json.{Format, Json}

object Metadata {
  import AwtJson._

  implicit val metadataFormat: Format[Metadata] = Json.format[Metadata]
}
case class Metadata(
  size: Dimension
)
