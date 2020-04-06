package services

import java.io.File

import javax.inject.{Inject, Singleton}
import play.api.Configuration

@Singleton
class WorkingDirectoryService @Inject()(cfg: Configuration) {
  private def createIfNotExist(dir: File): File = dir match {
    case existingDir: File if existingDir.exists =>
      existingDir
    case newDir: File =>
      newDir.mkdirs()
      newDir
  }

  val workingDir: File = createIfNotExist(
    new File(cfg.get[String]("puddings-cam.working-dir.path"))
  )
  val jpegCacheDir: File = createIfNotExist(
    new File(workingDir, "cache/jpeg")
  )
  val metadataCacheDir: File = createIfNotExist(
    new File(workingDir, "cache/metadata")
  )
  val trainingDir: File = createIfNotExist(
    new File(workingDir, "training/cascade")
  )
  val annotationsDir: File = createIfNotExist(
    new File(workingDir, "annotations")
  )
}
