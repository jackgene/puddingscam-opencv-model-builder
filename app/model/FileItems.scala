package model

import play.api.libs.json.{Json, Writes}

object FileItem {
  implicit val fileItemWrites: Writes[FileItem] = Json.writes[FileItem]
}
case class FileItem(name: String, dir: Boolean, numAnnotations: Option[Int] = None)

object FileItems {
  implicit val filesWrites: Writes[FileItems] = Json.writes[FileItems]
}
case class FileItems(
  path: Seq[String],
  fileItems: Seq[FileItem]
)
