package model

import java.awt.Rectangle

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json}

object Annotation {
  import AwtJson.rectangleFormat

  implicit val annotationFormat: Format[Annotation] = Json.format[Annotation]
}
case class Annotation(
  label: String,
  shape: Rectangle
)

object Annotations {
  implicit val annotationsFormat: Format[Annotations] = (
    (JsPath \ "annotations").format[Seq[Annotation]] and
    (JsPath \ "unused").formatNullable[String] // Play JSON needs at least two fields to use combinators
  )(
    (annotations: Seq[Annotation], _: Option[String]) => Annotations(annotations),
    (annotations: Annotations) => (annotations.annotations, None)
  )
}
case class Annotations(
  annotations: Seq[Annotation],
  lastSaved: Long = 0L
)
