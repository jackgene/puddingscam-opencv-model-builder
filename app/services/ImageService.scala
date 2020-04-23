package services

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.{BufferedWriter, File, FileInputStream, FileWriter}
import java.nio.file.{FileSystem, FileSystems, Files, Path, PathMatcher}

import javax.imageio.ImageIO
import javax.imageio.spi.{IIORegistry, ImageReaderSpi}
import javax.inject.{Inject, Singleton}
import javaxt.io.Image
import model.{Annotations, Metadata}
import org.libraw.javax.imageio.spi.LibRawImageReaderSpi
import play.api.libs.json.Json
import play.api.{Configuration, Logging}

import scala.io.Source
import scala.jdk.CollectionConverters._

@Singleton
class ImageService @Inject()(cfg: Configuration, workingDirs: WorkingDirectoryService) extends Logging {
  import workingDirs._

  {
    val iioReg: IIORegistry = IIORegistry.getDefaultInstance

    try {
      for {
        tiffReaderSpi: ImageReaderSpi <- Option(
          iioReg.getServiceProviderByClass(
            Class.
              forName("com.sun.imageio.plugins.tiff.TIFFImageReaderSpi").
              asSubclass(classOf[ImageReaderSpi])
          )
        )
        libRawReaderSpi: ImageReaderSpi <- Option(
          iioReg.getServiceProviderByClass(classOf[LibRawImageReaderSpi])
        )
      } iioReg.setOrdering(classOf[ImageReaderSpi], libRawReaderSpi, tiffReaderSpi)
    } catch {
      case _: ClassNotFoundException => // noop
      case t: Throwable =>
        logger.error("Unable to elevate LibRawImageReaderSpi in registry.", t)
    }
    logger.debug("ImageReaderSpi by priority:")
    for {
      spi: ImageReaderSpi <- iioReg.
        getServiceProviders(classOf[ImageReaderSpi], true).asScala
    } logger.debug(spi.getClass.getCanonicalName)
  }

  private val fs: FileSystem = FileSystems.getDefault
  private val isJson: PathMatcher = fs.getPathMatcher("glob:**/*.json")
  private val basePhotosDir: File = new File(cfg.get[String]("puddings-cam.base-photo-dir.path"))
  private val annotationsDirPath: Path = annotationsDir.toPath

  private def loadAndCacheImage(path: String): Unit = {
    val sourceImageFile = new File(basePhotosDir, path)

    if (sourceImageFile.exists) {
      val image: Image = new Image(sourceImageFile)

      val cacheImageFile = new File(jpegCacheDir, s"${path}.jpg")
      cacheImageFile.getParentFile.mkdirs()
      image.rotate() // Rotate JPEG if necessary
      image.saveAs(cacheImageFile)

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
        if (!cacheImageFile.exists) loadAndCacheImage(path)

        Option(ImageIO.read(cacheImageFile))

      case _ => None
    }
  }

  def getMetadata(path: String): Option[Metadata] = {
    new File(basePhotosDir, path) match {
      case sourceImageFile: File if sourceImageFile.exists =>
        import Metadata.metadataFormat

        val cacheMetadataFile = new File(metadataCacheDir, s"${path}.json")
        if (!cacheMetadataFile.exists) loadAndCacheImage(path)

        Some(Json.parse(new FileInputStream(cacheMetadataFile)).as[Metadata])

      case _ => None
    }
  }

  def getAllAnnotationsByPath: Iterator[(String,Annotations)] =
    for {
      path: Path <- Files.walk(annotationsDirPath).iterator.asScala
      if isJson.matches(path)
      annotationsPathSpec: String =
        annotationsDirPath.relativize(path).toString.dropRight(5)
      annotations: Annotations <- getAnnotations(annotationsPathSpec)
    } yield (annotationsPathSpec, annotations)

  def getAnnotations(path: String): Option[Annotations] = {
    new File(annotationsDir, s"${path}.json") match {
      case annotationFile: File if annotationFile.exists =>
        val src: Source = Source.fromFile(annotationFile)
        try {
          Option(
            Json.parse(src.mkString).as[Annotations].
              copy(lastSaved = annotationFile.lastModified)
          )
        } finally {
          src.close()
        }

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
