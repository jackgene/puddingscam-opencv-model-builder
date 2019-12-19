package model

import java.awt.Rectangle

import play.api.libs.json.{Format, Json}

object Annotation {
  import AwtJson.rectangleFormat

  implicit val annotationFormat: Format[Annotation] = Json.format[Annotation]
}
case class Annotation(
  label: String,
  shape: Rectangle
)

object Annotations {
  implicit val annotationsFormat: Format[Annotations] = Json.format[Annotations]
}
case class Annotations(
  annotations: Seq[Annotation]
)
