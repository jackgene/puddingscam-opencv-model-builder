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
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{FrameConverter, Java2DFrameConverter}
import org.bytedeco.opencv.opencv_core.{Mat, Rect, RectVector, Size}
import org.bytedeco.opencv.opencv_objdetect.CascadeClassifier
import play.api.Configuration
import play.api.Logging

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import scala.sys.process._

@Singleton
class DetectionService @Inject()(cfg: Configuration, imageService: ImageService) extends Logging {
  private val workingDir: File =
    new File(cfg.get[String]("puddings-cam.working-dir.path")) match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        newDir.mkdirs()
        newDir
    }
  private val trainingDir: File =
    new File(workingDir, "training/cascade") match {
      case existingDir: File if existingDir.exists =>
        existingDir
      case newDir: File =>
        new File(newDir, "bg").mkdirs()
        new File(newDir, "fg").mkdirs()
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
            OpenCvTrainCascadeOpts
        runWithLogging(trainCmd, labelTrainingDir, trainLogFile)
      }.
      takeWhile(_ != 0).
      size
    (s"echo Training complete after ${attempts} failed attempts" #>> trainLogFile).!
  }

  private val bufferedImageToFrame: FrameConverter[BufferedImage] = new Java2DFrameConverter()
  private val frameToMat: ToMat = new ToMat()
  private val faceClassifierOpt: Option[CascadeClassifier] =
    Option(
      new CascadeClassifier(
        s"${workingDir}/suggestion/cascade/face/cascade.xml"
      )
    ).
    filterNot(_.empty)
  private val eyeClassifierOpt: Option[CascadeClassifier] =
    Option(
      new CascadeClassifier(
        s"${workingDir}/suggestion/cascade/eye/cascade.xml"
      )
    ).
    filterNot(_.empty)
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
      classifier: CascadeClassifier, mat: Mat, minSize: Int, maxSize: Int): Seq[Rect] = {
    val rects: RectVector = new RectVector()
    classifier.detectMultiScale(
      mat, rects, 1.1, 3, 0,
      new Size(minSize, minSize), new Size(maxSize, maxSize)
    )

    (0L until rects.size).map(rects.get)
  }

  def detect(path: String): Option[Annotations] = {
    for {
      faceClassifier: CascadeClassifier <- faceClassifierOpt
      img: BufferedImage <- imageService.getImage(path)
      mat: Mat = frameToMat.convert(bufferedImageToFrame.convert(img))
      minSize: Int = math.min(mat.rows, mat.cols) / 20
      faces: Seq[Rect] <-
        openCvDetectMultiscale(faceClassifier, mat, minSize, mat.rows) match {
          case empty: Seq[Rect] if empty.isEmpty => None
          case nonEmpty: Seq[Rect] => Some(nonEmpty)
        }
      largestFace: Rect = faces.max
    } yield {
      val eyes: List[Annotation] =
        for {
          eyeClassifier: CascadeClassifier <- eyeClassifierOpt.toList
          faceMat: Mat = mat.apply(largestFace)
          rect: Rect <-
            openCvDetectMultiscale(eyeClassifier, faceMat, 0, faceMat.rows / 4).
            sorted.takeRight(2)
        } yield Annotation(
          label = "eye",
          shape = new Rectangle(
            largestFace.x + rect.x, largestFace.y + rect.y, rect.width, rect.height
          )
        )

      Annotations(
        annotations =
          Annotation(
            label = "face",
            shape = new Rectangle(
              largestFace.x, largestFace.y, largestFace.width, largestFace.height
            )
          ) :: eyes
      )
    }
  }
}
