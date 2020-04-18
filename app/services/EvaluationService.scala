package services

import java.awt.Rectangle
import java.io.File

import akka.stream.scaladsl.Source
import javax.inject.{Inject, Singleton}
import model.{Annotation, Annotations}
import play.api.Logging

@Singleton
class EvaluationService @Inject()(
    imageService: ImageService, detectionService: DetectionService,
    workingDirs: WorkingDirectoryService)
    extends Logging {
  import workingDirs._

  private val minNeighbors: Seq[Int] = 0 to 99

  private def area(rect: Rectangle): Double =
    if (rect.isEmpty) 0.0 else rect.getHeight * rect.getWidth

  private def intersectionOverUnion(rect1: Rectangle, rect2: Rectangle):
      Double = {
    val intersectArea: Double = area(rect1.intersection(rect2))

    intersectArea / (area(rect1) + area(rect2) - intersectArea)
  }

  private def intersectionOverUnion(rect: Rectangle, candidates: Seq[Rectangle]):
      Double = {
    import Ordering.Double.TotalOrdering

    candidates.map(intersectionOverUnion(rect, _)).maxOption.getOrElse(0.0)
  }

  def evaluate(label: String, modelName: String):
      Source[(Int,Double,Int), _] = {
    val annotationsByPath: Seq[(String, Seq[Rectangle])] = imageService.
      getAllAnnotationsByPath.
      map { case (path: String, annotations: Annotations) =>
        path -> annotations.annotations.collect {
          case Annotation(`label`, shape: Rectangle) => shape
        }
      }.
      toSeq
    val objectRects: Seq[Rectangle] = annotationsByPath.flatMap {
      case (_, rects: Seq[Rectangle]) => rects
    }
    logger.info(
      s"Evaluating ${objectRects.size} annotated ${label}s" +
      s" in ${annotationsByPath.size} images."
    )

    Source(minNeighbors).mapConcat { minNeighbor: Int =>
      DetectionService.OpenCvClassifier.
        create(
          new File(trainingDir, s"${label}/${modelName}/data/cascade.xml"),
          1.1, minNeighbor
        ).
        map { classifier: DetectionService.OpenCvClassifier =>
          val annotatedAndDetected: Seq[(Seq[Rectangle],Seq[Rectangle])] =
            for {
              (path: String, annotated: Seq[Rectangle]) <- annotationsByPath
            } yield (
              annotated,
              detectionService.classify(classifier, path, 0, 0)
            )
          val detectedCount: Int = annotatedAndDetected.
            map {
              case (_, detected: Seq[Rectangle]) => detected.size
            }.
            sum
          val intersectionsOverUnion: Seq[Double] =
            for {
              (annotateds: Seq[Rectangle], detecteds: Seq[Rectangle]) <-
                annotatedAndDetected
              detected: Rectangle <- detecteds match {
                case Nil => Seq(new Rectangle)
                case nonEmpty: Seq[Rectangle] => nonEmpty
              }
            } yield intersectionOverUnion(detected, annotateds)

          (
            minNeighbor,
            intersectionsOverUnion.sum / intersectionsOverUnion.size,
            detectedCount
          )
        }.
        toList
    }
  }
}
