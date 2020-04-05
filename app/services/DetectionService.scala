package services

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.{File, FileNotFoundException, FileWriter, PrintWriter}
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
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{FrameConverter, Java2DFrameConverter}
import play.api.{Configuration, Logging}

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import scala.sys.process._
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object DetectionService {
  object OpenCvClassifier {
    def create(parameters: File, scaleFactor: Double, minNeighbors: Int): Try[OpenCvClassifier] =
      if (parameters.exists && parameters.isFile)
        Try {
          new CudaOpenCvCascadeClassifier(parameters, scaleFactor, minNeighbors)
        }.recoverWith {
          case NonFatal(_) =>
            Try(new CpuOpenCvCascadeClassifier(parameters, scaleFactor, minNeighbors))
        }
      else
        Failure(new FileNotFoundException())
  }
  trait OpenCvClassifier {
    def detect(image: Mat, minSize: Size, maxSize: Size): RectVector
  }
  class CpuOpenCvCascadeClassifier(parameters: File, scaleFactor: Double, minNeighbors: Int)
    extends OpenCvClassifier with Logging {
    val classifier: CascadeClassifier = new CascadeClassifier(parameters.getCanonicalPath)
    logger.info(s"Loaded classifier from ${parameters}...")

    override def detect(image: Mat, minSize: Size, maxSize: Size): RectVector = {
      val objs = new RectVector
      classifier.detectMultiScale(image, objs, scaleFactor, minNeighbors, 0, minSize, maxSize)

      objs
    }
  }
  class CudaOpenCvCascadeClassifier(parameters: File, scaleFactor: Double, minNeighbors: Int)
      extends OpenCvClassifier with Logging {
    val classifier: CudaCascadeClassifier =
      CudaCascadeClassifier.create(parameters.getCanonicalPath)
    classifier.setScaleFactor(scaleFactor)
    classifier.setMinNeighbors(minNeighbors)
    val DefaultObjectSize = new Size(0, 0)
    logger.info(s"Loaded classifier from ${parameters}...")

    override def detect(image: Mat, minSize: Size, maxSize: Size): RectVector = {
      val objsMat: GpuMat = new GpuMat
      val objs = new RectVector
      classifier.synchronized {
        classifier.setMinObjectSize(minSize)
        classifier.setMaxObjectSize(maxSize)

        classifier.detectMultiScale(new GpuMat(image), objsMat)
        classifier.convert(objsMat, objs)

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

  private val AnnotationsDirPathChars: Int = annotationsDir.getAbsolutePath.length
  private val OpenCvTrainCascadeOpts: String =
    cfg.getOptional[String]("puddings-cam.opencv-traincascade.options").
    getOrElse("")

  private def getAllAnnotations(root: File): Seq[(String,Annotations)] = {
    root.listFiles match {
      case Array() => Nil

      case nonEmpty: Array[File] =>
        for {
          fileOrDir: File <- ArraySeq.unsafeWrapArray(nonEmpty)
          annotations: (String,Annotations) <-
            if (fileOrDir.isDirectory) getAllAnnotations(fileOrDir)
            else {
              val path: String = fileOrDir.getAbsolutePath.drop(AnnotationsDirPathChars + 1).dropRight(5)

              imageService.getAnnotations(path).map { annotations: Annotations => (path, annotations) }
            }
        } yield annotations
    }
  }

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
    val allAnnotations: Seq[(String, Annotations)] = getAllAnnotations(annotationsDir)

    // Prepare foreground images and metadata
    val fgDir = new File(labelTrainingDir, "fg")
    fgDir.mkdirs()
    for {
      (path: String, annotations: Annotations) <- allAnnotations
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
      (path: String, annotations: Annotations) <- allAnnotations
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
      "opencv_createsamples " +
      s"-vec ${new File(modelDir, "positive.vec").getAbsolutePath} " +
      s"-info info.dat " +
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
          "opencv_traincascade " +
            s"-data ${new File(modelName, "data").getPath} " +
            s"-vec ${new File(modelName, "positive.vec").getPath} " +
            "-bg bg.txt " +
            s"-numPos ${numPosTrain} -numNeg ${numNeg} -numStages ${numStages} " +
            s"-w ${objectSizePx} -h ${objectSizePx} -minHitRate ${minHitRate} " +
            "-baseFormatSave " + // Save in old format as required by CUDA classifier
            OpenCvTrainCascadeOpts
        runWithLogging(trainCmd, labelTrainingDir, trainLogFile)
      }.
      takeWhile(_ != 0).
      size
    (s"echo Training complete after ${attempts} failed attempts" #>> trainLogFile).!
  }

  private val bufferedImageToFrame: FrameConverter[BufferedImage] = new Java2DFrameConverter()
  private val frameToMat: ToMat = new ToMat()
  private val faceClassifierOpt: Option[OpenCvClassifier] =
    OpenCvClassifier.create(
      new File(s"${workingDir}/suggestion/cascade/face/cascade.xml"),
      1.1, 3
    ).toOption
  private val eyeClassifierOpt: Option[OpenCvClassifier] =
    OpenCvClassifier.create(
      new File(s"${workingDir}/suggestion/cascade/eye/cascade.xml"),
      1.1, 3
    ).toOption
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

  private implicit val RectOrdering: Ordering[Rect] =
    Ordering.by{ rect: Rect => rect.width * rect.height }
  private def openCvDetectMultiscale(
      classifier: OpenCvClassifier, mat: Mat, minSize: Int, maxSize: Int): Seq[Rect] = {
    val rects: RectVector = classifier.detect(
      mat, new Size(minSize, minSize), new Size(maxSize, maxSize)
    )

    (0L until rects.size).map(rects.get)
  }

  def detect(path: String): Option[Annotations] = {
    for {
      faceClassifier: OpenCvClassifier <- faceClassifierOpt
      mat: Mat = imread(new File(jpegCacheDir, s"${path}.jpg").getCanonicalPath, IMREAD_GRAYSCALE)
      minSize: Int = math.min(mat.rows, mat.cols) / 20
      faces: Seq[Rect] <-
        openCvDetectMultiscale(faceClassifier, mat, minSize, mat.rows) match {
          case empty: Seq[Rect] if empty.isEmpty => None
          case nonEmpty: Seq[Rect] => Some(nonEmpty)
        }
    } yield {
      val face: Rect = faces.max
      val eyes: List[Annotation] =
        for {
          eyeClassifier: OpenCvClassifier <- eyeClassifierOpt.toList
          eyeArea: Rect = new Rect(
            face.x, face.y + (face.height / 4), face.width, face.height * 7 / 15
          )
          eyeAreaMat: Mat = mat(eyeArea)
          rect: Rect <-
            openCvDetectMultiscale(eyeClassifier, eyeAreaMat, face.height / 10, face.height / 4).
            sorted.takeRight(2)
        } yield Annotation(
          label = "eye",
          shape = new Rectangle(
            eyeArea.x + rect.x, eyeArea.y + rect.y, rect.width, rect.height
          )
        )

      Annotations(
        annotations =
          Annotation(
            label = "face",
            shape = new Rectangle(
              face.x, face.y, face.width, face.height
            )
          ) :: eyes
      )
    }
  }
}
