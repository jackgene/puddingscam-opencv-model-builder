package model

import java.awt.{Dimension, Rectangle}

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath}

object AwtJson {
  implicit val dimensionFormat: Format[Dimension] = (
    (JsPath \ "widthPixel").format[Int] and
    (JsPath \ "heightPixel").format[Int]
  )(
    new Dimension(_, _),
    (dim: Dimension) => (dim.getWidth.toInt, dim.getHeight.toInt)
  )
  implicit val rectangleFormat: Format[Rectangle] = (
    (JsPath \ "leftPixel").format[Int] and
    (JsPath \ "topPixel").format[Int] and
    (JsPath \ "widthPixel").format[Int] and
    (JsPath \ "heightPixel").format[Int]
  )(
    new Rectangle(_, _, _, _),
    { rect: Rectangle =>
      (rect.getLocation.x, rect.getLocation.y, rect.getWidth.toInt, rect.getHeight.toInt)
    }
  )
}
