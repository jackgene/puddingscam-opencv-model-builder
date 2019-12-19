package model

import java.io.File

import play.api.libs.json.{Json, Writes}

object FileItem {
  implicit val fileItemWrites: Writes[FileItem] = Json.writes[FileItem]

  def apply(file: File): FileItem = FileItem(file.getName, file.isDirectory)
}
case class FileItem(name: String, dir: Boolean)

object FileItems {
  implicit val filesWrites: Writes[FileItems] = Json.writes[FileItems]
}
case class FileItems(
  path: Seq[String],
  fileItems: Seq[FileItem]
)
