package services

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Path}
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
import scala.jdk.CollectionConverters._
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
    val allAnnotations: Seq[(String, Annotations)] = imageService.getAllAnnotationsByPath.toSeq

    // Prepare foreground images and metadata
    val fgDir = new File(labelTrainingDir, "fg")
    fgDir.mkdirs()
    for {
      (path: String, annotations: Annotations) <- allAnnotations.par
      openCvPath: String = path.replace(" ", "%20") // OpenCV CLI tools can't handle spaces
      (annotation: Annotation, idx: Int) <- annotations.annotations.filter(_.label == label).zipWithIndex
      outFile = new File(fgDir, s"${openCvPath}.${idx}.jpg")
      if !outFile.exists() || outFile.lastModified < annotations.lastSaved
      _ = outFile.getParentFile.mkdirs()
      imgInfoFileWriter = new PrintWriter(new FileWriter(new File(fgDir, s"${openCvPath}.${idx}.info.dat")))
      img: BufferedImage <- imageService.getImage(path)
    } {
      val rect: Rectangle = annotation.shape
      ImageIO.write(img.getSubimage(rect.x, rect.y, rect.width, rect.height), "jpeg", outFile)
      try {
        imgInfoFileWriter.println(
          s"${outFile.getAbsolutePath} 1 0 0 ${rect.width} ${rect.height}"
        )
      } finally {
        imgInfoFileWriter.close()
      }
    }
    val infoFileWriter = new PrintWriter(new FileWriter(new File(labelTrainingDir, "info.dat")))
    val numPos: Int =
      try {
        Files.walk(fgDir.toPath).iterator.asScala.
          filter(_.getFileName.toString.endsWith("info.dat")).
          flatMap { path: Path =>
            Files.readAllLines(path).asScala
          }.
          foldLeft(0) { (count: Int, line: String) =>
            infoFileWriter.println(line)
            count + 1
          }
      } finally {
        infoFileWriter.close()
      }

    // Prepare background images and metadata
    val bgDir = new File(labelTrainingDir, "bg")
    bgDir.mkdirs()
    for {
      (path: String, annotations: Annotations) <- allAnnotations.par
      rects: Set[Rectangle] = annotations.annotations.filter(_.label == label).map(_.shape).toSet
      if rects.nonEmpty
      outDir = new File(bgDir, path.replace(" ", "%20")) // OpenCV CLI tools can't handle spaces
      _ = if (outDir.exists && outDir.lastModified < annotations.lastSaved)
        Files.walk(outDir.toPath).
        sorted(Comparator.reverseOrder()).
        forEach(Files.delete _)
      if !outDir.exists()
      _ = outDir.mkdirs()
      img: BufferedImage <- imageService.getImage(path)
      size: Int = rects.map(_.width).max
      step: Int = size / 2
      margin: Int = size * 3 / 4
      bounds: Rectangle <-
        if (label == "eye") annotations.annotations.find(_.label == "face").map(_.shape)
        else Option(img.getRaster.getBounds)
    } {
      val imgBgFileWriter = new PrintWriter(new File(outDir, "bg.txt"))
      try {
        for {
          relX: Int <- 0 to (bounds.width - size) by step
          x = relX + bounds.x
          relY: Int <- 0 to (bounds.height - size) by step
          y = relY + bounds.y
          if rects.forall { rect: Rectangle =>
            x < rect.x - margin || x >= rect.x + margin || y < rect.y - margin || y >= rect.y + margin
          }
        } {
          val imgOutFile = new File(outDir, f"y${y}%04d_x${x}%04d.jpg")
          ImageIO.write(img.getSubimage(x, y, size, size), "jpeg", imgOutFile)
          imgBgFileWriter.println(imgOutFile.getAbsolutePath)
        }
      } finally {
        imgBgFileWriter.close()
      }
    }
    val bgFileWriter = new PrintWriter(new FileWriter(new File(labelTrainingDir, "bg.txt")))
    val numNeg: Int =
      try {
        import scala.jdk.CollectionConverters._
        Files.walk(bgDir.toPath).iterator.asScala.
          filter(_.endsWith("bg.txt")).
          flatMap { path: Path =>
            Files.readAllLines(path).asScala
          }.
          foldLeft(0) { (count: Int, line: String) =>
            bgFileWriter.println(line)
            count + 1
          }
      } finally {
        bgFileWriter.close()
      }

    val modelName = s"model_${objectSizePx}x${objectSizePx}_${LocalDateTime.now}"
    val modelDir = new File(labelTrainingDir, modelName)
    new File(modelDir, "data").mkdirs()

    val vecCmd =
      s"${openCvBinDir}/opencv_createsamples " +
      s"-info info.dat " +
      s"-vec ${new File(modelDir, "positive.vec").getAbsolutePath} " +
      s"-num ${numPos} " +
      s"-w ${objectSizePx} -h ${objectSizePx}"
    val vecLogFile = new File(modelDir, "opencv_createsamples.log")
    runWithLogging(vecCmd, labelTrainingDir, vecLogFile)

    val numStages = 20
    val minHitRate = 0.995
    // From https://stackoverflow.com/questions/10863560/haar-training-opencv-assertion-failed
    val trainLogFile = new File(modelDir, "opencv_traincascade.log")
    val attempts: Int =
      ((numPos / (1 + ((numStages - 1) * (1 - minHitRate)))).toInt to (numPos * 0.8).toInt by -1).iterator.
      map { numPosTrain: Int =>
        val trainCmd =
          s"${openCvBinDir}/opencv_traincascade " +
          s"-data ${new File(modelName, "data").getPath} " +
          s"-vec ${new File(modelName, "positive.vec").getPath} " +
          "-bg bg.txt " +
          s"-numPos ${numPosTrain} -numNeg ${numNeg} -numStages ${numStages} " +
          s"-w ${objectSizePx} -h ${objectSizePx} -minHitRate ${minHitRate} " +
          "-baseFormatSave " + // Save in old format as required by CUDA classifier
          openCvTrainCascadeOpts
        runWithLogging(trainCmd, labelTrainingDir, trainLogFile)
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
