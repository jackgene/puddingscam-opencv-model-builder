package services

import java.awt.Rectangle
import java.io.File

import akka.stream.scaladsl.Source
import javax.inject.{Inject, Singleton}
import model.{Annotation, Annotations}
import play.api.Logging

import scala.util.Failure

@Singleton
class EvaluationService @Inject()(
    imageService: ImageService, detectionService: DetectionService,
    workingDirs: WorkingDirectoryService)
    extends Logging {
  import workingDirs._

  private def minNeighbors: Seq[Int] =
    LazyList.iterate(1.0)(_ * 1.1).takeWhile(_ < 500).map(_.toInt).distinct

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
      Source[(Int,Double,Int,Int,Seq[(String,Double)]), _] = {
    val annotationsByPath: Seq[(String, Seq[Rectangle])] = imageService.
      getAllAnnotationsByPath.
      collect {
        case (path: String, annotations: Annotations)
            if Math.abs(path.hashCode % 5) >= 4 =>
          path -> annotations.annotations.collect {
            case Annotation(`label`, shape: Rectangle) => shape
          }
      }.
      toSeq
    val objectRects: Seq[Rectangle] = annotationsByPath.flatMap {
      case (_, rects: Seq[Rectangle]) => rects
    }
    logger.info(
      s"Evaluating model against ${objectRects.size} annotated ${label}s" +
      s" in ${annotationsByPath.size} images."
    )

    Source(minNeighbors).mapConcat { minNeighbor: Int =>
      DetectionService.OpenCvClassifier.
        create(
          new File(trainingDir, s"${label}/${modelName}/data/cascade.xml"),
          1.1, minNeighbor
        ).
        map { classifier: DetectionService.OpenCvClassifier =>
          val annotatedAndDetected: Seq[(String,Seq[Rectangle],Seq[Rectangle],Int)] =
            for {
              (path: String, annotateds: Seq[Rectangle]) <- annotationsByPath
              detecteds: Seq[Rectangle] <- {
                logger.debug(s"Classifying ${label}s in ${path}.")
                detectionService.
                  classify(classifier, path, 0, 0).
                  recoverWith {
                    case t: Throwable =>
                      logger.warn("Classifying failed.", t)
                      Failure(t)
                  }.
                  toOption
              }
            } yield {
              val detectedCount: Int = detecteds.size
              logger.debug(s"Found ${detectedCount} ${label}s.")

              (path, annotateds, detecteds, detectedCount)
            }
          val detectedCount: Int = annotatedAndDetected.
            map {
              case (_, _, _, count: Int) => count
            }.
            sum
          logger.debug(s"Found ${detectedCount} total ${label}s")
          val iousByPath: Seq[(String,Double)] =
            for {
              (path: String, annotateds: Seq[Rectangle], detecteds: Seq[Rectangle], _) <-
                annotatedAndDetected
              detected: Rectangle <- detecteds match {
                case Nil => Seq(new Rectangle)
                case nonEmpty: Seq[Rectangle] => nonEmpty
              }
            } yield path -> intersectionOverUnion(detected, annotateds)

          def meanIou(iousByPath: Seq[(String,Double)]): Double =
            iousByPath.map { case (_, iou: Double) => iou }.sum / iousByPath.size
          (
            minNeighbor,
            meanIou(iousByPath),
            annotatedAndDetected.size,
            detectedCount,
            iousByPath.
              groupBy {
                case (path: String, _) => path
              }.
              view.
              mapValues(meanIou).
              toSeq
          )
        }.
        toList
    }
  }
}
