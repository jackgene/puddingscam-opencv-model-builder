package services

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.{BufferedWriter, File, FileInputStream, FileWriter}

import javax.imageio.ImageIO
import javax.inject.{Inject, Singleton}
import model.{Annotations, Metadata}
import play.api.Configuration
import play.api.libs.json.Json

import scala.io.Source

@Singleton
class ImageService @Inject()(cfg: Configuration) {
  private val workingDir: File =
    new File(cfg.get[String]("puddings-cam.working-dir.path")) match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        newDir.mkdirs()
        newDir
    }
  private val jpegCacheDir: File =
    new File(workingDir, "cache/jpeg") match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        newDir.mkdirs()
        newDir
    }
  private val metadataCacheDir: File =
    new File(workingDir, "cache/metadata") match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        newDir.mkdirs()
        newDir
    }
  private val annotationsDir: File =
    new File(workingDir, "annotations") match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        newDir.mkdirs()
        newDir
    }
  private val basePhotosDir: File = new File(cfg.get[String]("puddings-cam.base-photo-dir.path"))

  private def loadCachedImage(path: String): Unit = {
    val sourceImageFile = new File(basePhotosDir, path)

    if (sourceImageFile.exists) {
      val image: BufferedImage = ImageIO.read(sourceImageFile)

      val cacheImageFile = new File(jpegCacheDir, s"${path}.jpg")
      cacheImageFile.getParentFile.mkdirs()
      ImageIO.write(image, "jpg", cacheImageFile)

      val cacheMetadataFile = new File(metadataCacheDir, s"${path}.json")
      cacheMetadataFile.getParentFile.mkdirs()
      val cacheMetadataWriter = new FileWriter(cacheMetadataFile)
      try {
        cacheMetadataWriter.write(
          Json.toJson(Metadata(new Dimension(image.getWidth, image.getHeight))).toString()
        )
      } finally {
        cacheMetadataWriter.close()
      }
    }
  }

  def getImage(path: String): Option[BufferedImage] = {
    new File(basePhotosDir, path) match {
      case sourceImageFile: File if sourceImageFile.exists =>
        val cacheImageFile = new File(jpegCacheDir, s"${path}.jpg")
        if (!cacheImageFile.exists) loadCachedImage(path)

        Some(ImageIO.read(cacheImageFile))

      case _ => None
    }
  }

  def getMetadata(path: String): Option[Metadata] = {
    new File(basePhotosDir, path) match {
      case sourceImageFile: File if sourceImageFile.exists =>
        import Metadata.metadataFormat

        val cacheMetadataFile = new File(metadataCacheDir, s"${path}.json")
        if (!cacheMetadataFile.exists) loadCachedImage(path)

        Some(Json.parse(new FileInputStream(cacheMetadataFile)).as[Metadata])

      case _ => None
    }
  }

  def getAnnotations(path: String): Option[Annotations] = {
    new File(annotationsDir, s"${path}.json") match {
      case annotationFile: File if annotationFile.exists =>
        Option(Json.parse(Source.fromFile(annotationFile).mkString).as[Annotations])

      case _ => None
    }
  }

  def updateAnnotations(path: String, annotations: Annotations): Annotations = {
    import Annotations.annotationsFormat

    val annotationFile: File =
      new File(annotationsDir, s"${path}.json") match {
        case existingFile: File if existingFile.exists =>
          existingFile
        case newFile: File =>
          val parent = newFile.getParentFile
          if (!parent.exists) parent.mkdirs()
          newFile.createNewFile()
          newFile
      }
    val writer = new BufferedWriter(new FileWriter(annotationFile))
    try {
      writer.write(Json.toJson(annotations).toString())
      annotations
    } finally {
      writer.close()
    }
  }
}
