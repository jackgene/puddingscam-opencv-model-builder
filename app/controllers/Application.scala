package controllers

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import javax.activation.MimetypesFileTypeMap
import javax.imageio.ImageIO
import javax.inject._
import model.{Annotations, FileItem, FileItems, Metadata}
import play.api.Configuration
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import play.utils.UriEncoding
import services.{DetectionService, ImageService, SuggestionService}

import scala.jdk.CollectionConverters._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class Application @Inject()(
    cfg: Configuration, cc: ControllerComponents,
    imageService: ImageService, detectionService: DetectionService, suggestionService: SuggestionService)
    extends AbstractController(cc) {
  import Annotations.annotationsFormat
  import FileItems.filesWrites
  import Metadata.metadataFormat

  private val CacheMaxAge: Int = 365 * 24 * 60 * 60
  private val basePhotosDir: Path = Paths.get(cfg.get[String]("puddings-cam.base-photo-dir.path"))
  private val mimeTypeMap = new MimetypesFileTypeMap()
  private val imageFileSuffixes: Set[String] = ImageIO.getReaderFileSuffixes.toSet

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def listFiles(urlEncodedPath: String): Action[AnyContent] = Action {
    val path: String = UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8)
    Option(
      new File(basePhotosDir.toFile, path).listFiles { file: File =>
        val name: String = file.getName

        !name.startsWith(".") && (file.isDirectory || imageFileSuffixes.exists(name.toLowerCase.endsWith _))
      }
    ) match {
      case None => NotFound

      case Some(files: Array[File]) => Ok {
        Json.toJson(
          FileItems(
            path = Paths.get(path).normalize.iterator.asScala.map(_.getFileName.toString).filter(_.nonEmpty).toSeq,
            fileItems = files.toSeq.map { file: File =>
              FileItem(
                file.getName, file.isDirectory,
                if (file.isDirectory) None
                else
                  imageService.
                    getAnnotations(s"${path}${file.getName}").
                    map(_.annotations.size)
              )
            }
          )
        )
      }
    }
  }

  def showImage(urlEncodedPath: String): Action[AnyContent] = Action {
    val path: String = UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8)
    val lastDotIdx = path.lastIndexOf('.')

    imageService.getImage(path.substring(0, lastDotIdx)) match {
      case Some(image: BufferedImage) =>
        val outputFormat = path.substring(path.lastIndexOf('.') + 1)
        val buffer = new ByteArrayOutputStream()

        ImageIO.write(image, outputFormat, buffer)
        Ok(buffer.toByteArray).
          as(mimeTypeMap.getContentType(path)).
          withHeaders(CACHE_CONTROL -> s"max-age=${CacheMaxAge}")

      case None => NotFound
    }
  }

  def showMetadata(urlEncodedPath: String): Action[AnyContent] = Action {
    imageService.getMetadata(UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8)) match {
      case Some(metadata: Metadata) =>
        Ok(Json.toJson(metadata)).withHeaders(CACHE_CONTROL -> s"max-age=${CacheMaxAge}")

      case None => NotFound
    }
  }

  def showAnnotations(urlEncodedPath: String, suggested: Boolean): Action[AnyContent] = Action {
    val path: String = UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8)
    (if (suggested) suggestionService.suggestAnnotations(path) else imageService.getAnnotations(path)) match {
      case Some(annotations: Annotations) =>
        Ok(Json.toJson(annotations))

      case None => NotFound
    }
  }

  def newAnnotations(urlEncodedPath: String): Action[JsValue] = Action(parse.json) {
    implicit request: Request[JsValue] =>

    Ok(
      Json.toJson(
        imageService.updateAnnotations(
          UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8),
          request.body.as[Annotations]
        )
      )
    )
  }

  def deleteAnnotations(urlEncodedPath: String): Action[AnyContent] = Action {
    if (imageService.deleteAnnotations(UriEncoding.decodePath(urlEncodedPath, StandardCharsets.UTF_8)))
      NoContent
    else
      NotFound
  }

  def trainModel(label: String, objectSizePx: Int): Action[AnyContent] = Action {
    detectionService.trainModel(label, objectSizePx)
    Ok(s"Trained model for ${label} with object size ${objectSizePx}")
  }
}
