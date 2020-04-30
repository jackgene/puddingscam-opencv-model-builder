package services

import java.awt.Rectangle
import java.io.File

import javax.inject.{Inject, Singleton}
import model.{Annotation, Annotations, Metadata}
import play.api.{Configuration, Logging}
import services.DetectionService.OpenCvClassifier

@Singleton
class SuggestionService @Inject()(
    cfg: Configuration, imageService: ImageService, detectionService: DetectionService,
    workingDirs: WorkingDirectoryService)
    extends Logging {
  import workingDirs._

  private val suggestionCfg: Configuration =
    cfg.get[Configuration]("puddings-cam.suggestion")
  private val faceClassifierOpt: Option[OpenCvClassifier] =
    detectionService.loadClassifier(
      new File(workingDir, "suggestion/cascade/face/cascade.xml"),
      suggestionCfg.get[Double]("face.scale-factor"),
      suggestionCfg.get[Int]("face.min-neighbors")
    )
  private val eyeClassifierOpt: Option[OpenCvClassifier] =
    detectionService.loadClassifier(
      new File(workingDir, "suggestion/cascade/eye/cascade.xml"),
      suggestionCfg.get[Double]("eye.scale-factor"),
      suggestionCfg.get[Int]("eye.min-neighbors")
    )
  faceClassifierOpt match {
    case None =>
      logger.warn("face cascade classifier not loaded, annotations suggestions will not be available")

    case Some(_) =>
      eyeClassifierOpt match {
        case None =>
          logger.warn("eye cascade classifier not loaded, annotations suggestions for faces only")

        case Some(_) =>
          logger.info("All cascade classifiers for suggestions loaded")
      }
  }
  private implicit val RectangleOrdering: Ordering[Rectangle] =
    Ordering.by { rect: Rectangle => rect.width * rect.height }

  def suggestAnnotations(path: String): Option[Annotations] = {
    for {
      faceClassifier: OpenCvClassifier <- faceClassifierOpt
      metadata: Metadata <- imageService.getMetadata(path)
      minWidth: Int = math.min(metadata.size.width, metadata.size.height) / 20
      faceRects: Seq[Rectangle] <- detectionService.
        classify(faceClassifier, path, None, Some(minWidth), None).toOption
      if faceRects.nonEmpty
    } yield {
      val faceRect: Rectangle = faceRects.max
      val eyes: List[Annotation] =
        for {
          eyeClassifier: OpenCvClassifier <- eyeClassifierOpt.toList
          eyeArea: Rectangle = new Rectangle(
            faceRect.x, faceRect.y + (faceRect.height / 4), faceRect.width, faceRect.height * 7 / 15
          )
          minWidth: Int = faceRect.height / 10
          maxWidth: Int = faceRect.height / 4
          eyeRects: Seq[Rectangle] <- detectionService.
            classify(eyeClassifier, path, Some(eyeArea), Some(minWidth), Some(maxWidth)).
              toOption.toList
          eyeRect: Rectangle <- eyeRects.sorted.takeRight(2)
        } yield Annotation(
          label = "eye",
          shape = new Rectangle(
            eyeArea.x + eyeRect.x, eyeArea.y + eyeRect.y, eyeRect.width, eyeRect.height
          )
        )

      Annotations(
        annotations =
          Annotation(
            label = "face",
            shape = new Rectangle(
              faceRect.x, faceRect.y, faceRect.width, faceRect.height
            )
          ) :: eyes
      )
    }
  }
}
