package services

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Files
import java.time.LocalDateTime
import java.util.Comparator

import javax.imageio.ImageIO
import javax.inject.{Inject, Singleton}
import model.{Annotation, Annotations}
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_cudaobjdetect.CudaCascadeClassifier
import org.bytedeco.javacpp.opencv_imgcodecs.{IMREAD_GRAYSCALE, imread}
import org.bytedeco.javacpp.opencv_objdetect.CascadeClassifier
import play.api.{Configuration, Logging}

import scala.collection.parallel.CollectionConverters._
import scala.sys.process._
import scala.util.Try

object DetectionService {
  private object OpenCvClassifier {
    def create(parameters: File, scaleFactor: Double, minNeighbors: Int): Option[OpenCvClassifier] =
      if (parameters.exists && parameters.isFile)
        try {
          Some(new CudaOpenCvCascadeClassifier(parameters, scaleFactor, minNeighbors))
        } catch {
          case _: Error | _: RuntimeException =>
            Some(new CpuOpenCvCascadeClassifier(parameters, scaleFactor, minNeighbors))
        }
      else None
  }
  trait OpenCvClassifier {
    def detect(image: Mat, minSize: Size, maxSize: Size): Try[RectVector]
  }

  private class CpuOpenCvCascadeClassifier(parameters: File, scaleFactor: Double, minNeighbors: Int)
    extends OpenCvClassifier with Logging {
    val classifier: CascadeClassifier = new CascadeClassifier(parameters.getCanonicalPath)
    logger.info(s"Loaded classifier from ${parameters}...")

    override def detect(image: Mat, minSize: Size, maxSize: Size): Try[RectVector] = Try {
      val objs = new RectVector
      classifier.detectMultiScale(image, objs, scaleFactor, minNeighbors, 0, minSize, maxSize)

      objs
    }
  }

  private object CudaOpenCvCascadeClassifier {
    private val objsGpuMat: GpuMat = new GpuMat
    private val imageGpuMat: GpuMat = new GpuMat
  }
  private class CudaOpenCvCascadeClassifier(parameters: File, scaleFactor: Double, minNeighbors: Int)
      extends OpenCvClassifier with Logging {
    import CudaOpenCvCascadeClassifier._

    val DefaultObjectSize = new Size(0, 0)
    private val classifier: CudaCascadeClassifier =
      CudaCascadeClassifier.create(parameters.getCanonicalPath)
    classifier.setMaxNumObjects(1000000)
    classifier.setScaleFactor(scaleFactor)
    classifier.setMinNeighbors(minNeighbors)
    logger.info(s"Loaded classifier from ${parameters}...")

    override def detect(image: Mat, minSize: Size, maxSize: Size): Try[RectVector] = Try {
      val objs = new RectVector
      CudaOpenCvCascadeClassifier.synchronized {
        classifier.setMinObjectSize(minSize)
        classifier.setMaxObjectSize(maxSize)

        imageGpuMat.upload(image)
        classifier.detectMultiScale(imageGpuMat, objsGpuMat)
        classifier.convert(objsGpuMat, objs)

        classifier.setMinObjectSize(DefaultObjectSize)
        classifier.setMaxObjectSize(DefaultObjectSize)
      }

      objs
    }
  }
}
@Singleton
class DetectionService @Inject()(
    cfg: Configuration, imageService: ImageService, workingDirs: WorkingDirectoryService)
    extends Logging {
  import DetectionService._
  import workingDirs._

  private val detectionCfg: Configuration =
    cfg.get[Configuration]("puddings-cam.detection")
  private val openCvBinDir: String =
    detectionCfg.get[String]("train.opencv.bin-dir")
  private val openCvTrainCascadeOpts: String =
    detectionCfg.get[String]("train.opencv.traincascade.options")
  private val SizeFactors: Seq[Double] = (-1 to 5).map(math.pow(2, _))

  private def runWithLogging(cmd: String, workingDir: File, logFile: File): Int = {
    (s"echo Command: ${cmd}" #>> logFile).!
    val logWriter = new PrintWriter(new FileWriter(logFile, /*append=*/true))
    try {
      Process(cmd, workingDir).!(ProcessLogger { line: String => logWriter.println(line) })
    } finally {
      logWriter.close()
    }
  }

  def trainModel(label: String, objectSizePx: Int): Unit = {
    val labelTrainingDir = new File(trainingDir, label)
    if (!labelTrainingDir.exists) labelTrainingDir.mkdirs()
    val modelName = s"model_${objectSizePx}x${objectSizePx}_${LocalDateTime.now}"
    val modelDir = new File(labelTrainingDir, modelName)
    new File(modelDir, "data").mkdirs()

    val allPathsAndAnnotations: Seq[(String, Annotations)] = imageService.
      getAllAnnotationsByPath.
      filter {
        case (path: String, _: Annotations) => math.abs(path.hashCode % 5) < 4
      }.
      toSeq

    // Prepare foreground images and metadata
    val infoFile: File = new File(modelDir, "info.dat")
    val numPos: Int = {
      val infoFileWriter = new PrintWriter(new FileWriter(infoFile))
      val fgDir = new File(labelTrainingDir, "fg")
      fgDir.mkdirs()
      try {
        (
          for {
            (path: String, annotations: Annotations) <- allPathsAndAnnotations.par
            (annotation: Annotation, idx: Int) <- annotations.annotations.
              filter(_.label == label).zipWithIndex
            img: BufferedImage <- imageService.getImage(path)
          } yield {
            val rect: Rectangle = annotation.shape
            val openCvPath: String = path.replace(" ", "%20") // OpenCV CLI tools can't handle spaces
            val outFile: File = new File(fgDir, s"${openCvPath}.${idx}.jpg")

            if (!outFile.exists() || outFile.lastModified < annotations.lastSaved) {
              outFile.getParentFile.mkdirs()
              ImageIO.write(
                img.getSubimage(rect.x, rect.y, rect.width, rect.height), "jpeg", outFile
              )
            }
            infoFileWriter.println(
              s"${outFile.getAbsolutePath} 1 0 0 ${rect.width} ${rect.height}"
            )
          }
        ).size
      } finally {
        infoFileWriter.close()
      }
    }

    // Prepare background images and metadata
    val bgFile: File = new File(modelDir, "bg.txt")
    val numNeg: Int = {
      val bgFileWriter = new PrintWriter(new FileWriter(bgFile))
      val bgDir = new File(labelTrainingDir, "bg")
      bgDir.mkdirs()
      try {
        (
          for {
            (path: String, annotations: Annotations) <- allPathsAndAnnotations.par
            rects: Set[Rectangle] = annotations.annotations.filter(_.label == label).map(_.shape).toSet
            if rects.nonEmpty
            img: BufferedImage <- imageService.getImage(path)
            bounds: Rectangle <-
              if (label == "eye") annotations.annotations.find(_.label == "face").map(_.shape)
              else Option(img.getRaster.getBounds)
          } yield {
            val outDir: File = new File(bgDir, path.replace(" ", "%20")) // OpenCV CLI tools can't handle spaces
            val baseSize: Int = rects.map(_.width).max
            val boundsShortSide: Int = math.min(bounds.width, bounds.height)
            val negs: Seq[(Int,Int,Int,File)] =
              for {
                size: Int <- SizeFactors.
                  map { factor: Double => (factor * baseSize).toInt }.
                  dropWhile(_ <= boundsShortSide / 10).
                  takeWhile(_ <= boundsShortSide)
                step: Int = size / 2
                margin: Int = size * 3 / 4
                relX: Int <- 0 to (bounds.width - size) by step
                x = relX + bounds.x
                relY: Int <- 0 to (bounds.height - size) by step
                y = relY + bounds.y
                if size != baseSize || rects.forall { rect: Rectangle =>
                  x < rect.x - margin || x >= rect.x + margin || y < rect.y - margin || y >= rect.y + margin
                }
              } yield {
                val imgOutFile: File = new File(outDir, f"sz${size}%04d_y${y}%04d_x${x}%04d.jpg")
                bgFileWriter.println(imgOutFile.getAbsolutePath)
                (size, x, y, imgOutFile)
              }

            if (!outDir.exists || outDir.lastModified < annotations.lastSaved) {
              if (outDir.exists) Files.
                walk(outDir.toPath).
                sorted(Comparator.reverseOrder()).
                forEach(Files.delete _)
              outDir.mkdirs()

              for ((size: Int, x: Int, y: Int, imgOutFile: File) <- negs) {
                ImageIO.write(img.getSubimage(x, y, size, size), "jpeg", imgOutFile)
              }
            }

            negs.size
          }
        ).sum
      } finally {
        bgFileWriter.close()
      }
    }

    // Create positive samples
    val vecCmd =
      s"${openCvBinDir}/opencv_createsamples " +
      s"-info info.dat " +
      s"-vec positive.vec " +
      s"-num ${numPos} " +
      s"-w ${objectSizePx} -h ${objectSizePx}"
    val vecLogFile = new File(modelDir, "opencv_createsamples.log")
    runWithLogging(vecCmd, modelDir, vecLogFile)

    val numStages = 20
    val minHitRate = 0.995
    // From https://stackoverflow.com/questions/10863560/haar-training-opencv-assertion-failed
    val trainLogFile = new File(modelDir, "opencv_traincascade.log")
    val attempts: Int =
      ((numPos / (1 + ((numStages - 1) * (1 - minHitRate)))).toInt to (numPos * 0.8).toInt by -1).iterator.
      map { numPosTrain: Int =>
        val trainCmd =
          s"${openCvBinDir}/opencv_traincascade " +
          s"-data data " +
          s"-vec positive.vec " +
          s"-bg bg.txt " +
          s"-numPos ${numPosTrain} -numNeg ${numNeg} -numStages ${numStages} " +
          s"-w ${objectSizePx} -h ${objectSizePx} -minHitRate ${minHitRate} " +
          "-baseFormatSave " + // Save in old format as required by CUDA classifier
          openCvTrainCascadeOpts
        runWithLogging(trainCmd, modelDir, trainLogFile)
      }.
      takeWhile(_ != 0).
      size
    (s"echo Training complete after ${attempts} failed attempts" #>> trainLogFile).!
  }

  def loadClassifier(parameters: File, scaleFactor: Double, minNeighbors: Int):
      Option[OpenCvClassifier] =
    OpenCvClassifier.create(parameters, scaleFactor, minNeighbors)

  private def openCvDetectMultiscale(
      classifier: OpenCvClassifier, mat: Mat, minSize: Size, maxSize: Size):
      Try[Seq[Rect]] =
    classifier.detect(mat, minSize, maxSize).
      map { rects: RectVector =>
        (0L until rects.size).to(LazyList).map(rects.get)
      }

  def classify(
      classifier: OpenCvClassifier, path: String, crop: Option[Rectangle],
      minWidth: Option[Int] = None, maxWidth: Option[Int] = None):
      Try[Seq[Rectangle]] =
    openCvDetectMultiscale(
      classifier,
      {
        val mat = imread(
          new File(jpegCacheDir, s"${path}.jpg").getCanonicalPath,
          IMREAD_GRAYSCALE
        )

        crop match {
          case Some(rectangle: Rectangle) =>
            mat(new Rect(rectangle.x, rectangle.y, rectangle.width, rectangle.height))

          case None =>
            mat
        }
      },
      minWidth.map { width: Int => new Size(width, width) }.orNull,
      maxWidth.map { width: Int => new Size(width, width) }.orNull
    ).
    map { rects: Seq[Rect] =>
      rects.map { r: Rect => new Rectangle(r.x, r.y, r.width, r.height) }
    }
}
