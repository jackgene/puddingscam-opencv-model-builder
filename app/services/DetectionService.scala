package services

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.{File, FileWriter, PrintWriter}

import javax.imageio.ImageIO
import javax.inject.{Inject, Singleton}
import model.{Annotation, Annotations}
import play.api.Configuration
import play.api.libs.json.Json

import scala.collection.immutable.ArraySeq
import scala.io.Source
import scala.sys.process._

@Singleton
class DetectionService @Inject()(cfg: Configuration, imageService: ImageService) {
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

  private def getAllAnnotations(root: File): Seq[(String,Annotations)] = {
    root.listFiles match {
      case Array() => Nil

      case nonEmpty: Array[File] =>
        for {
          fileOrDir: File <- ArraySeq.unsafeWrapArray(nonEmpty)
          annotations: (String,Annotations) <-
            if (fileOrDir.isDirectory) getAllAnnotations(fileOrDir)
            else Seq(
              fileOrDir.getAbsolutePath.drop(AnnotationsDirPathChars + 1).dropRight(5) -> Json.parse(Source.fromFile(fileOrDir).mkString).as[Annotations]
            )
        } yield annotations
    }
  }

  def trainModel(objectSizePx: Int): Unit = {
    val allAnnotations: Seq[(String, Annotations)] = getAllAnnotations(annotationsDir)

    val infoFile = new File(trainingDir, "info.dat")
    val infoFileWriter = new PrintWriter(new FileWriter(infoFile, /*append=*/true))
    try {
      val fgDir = new File(trainingDir, "fg")

      for {
        (path: String, annotations: Annotations) <- allAnnotations
        outFile = new File(fgDir, path.replace(" ", "%20") + ".jpg") // OpenCV CLI tools can't handle spaces
        if !outFile.exists()
        _ = outFile.getParentFile.mkdirs()
        img: BufferedImage <- imageService.getImage(path)
        annotation: Annotation <- annotations.annotations
      } {
        val rect: Rectangle = annotation.shape
        ImageIO.write(img.getSubimage(rect.x, rect.y, rect.width, rect.height), "jpeg", outFile)
        infoFileWriter.println(
          s"${outFile.getAbsolutePath} 1 0 0 ${rect.width} ${rect.height}"
        )
      }
    } finally {
      infoFileWriter.close()
    }
    val numPos: Int = Source.fromFile(infoFile).getLines().length

    val bgFile = new File(trainingDir, "bg.txt")
    val bgFileWriter = new PrintWriter(new FileWriter(bgFile, /*append=*/true))
    try {
      val bgDir = new File(trainingDir, "bg")

      for {
        (path: String, annotations: Annotations) <- allAnnotations
        outDir = new File(bgDir, path.replace(" ", "%20")) // OpenCV CLI tools can't handle spaces
        if !outDir.exists()
        _ = outDir.mkdirs()
        img: BufferedImage <- imageService.getImage(path)
        annotation: Annotation <- annotations.annotations
        rect: Rectangle = annotation.shape
        step: Int = rect.width / 2
        margin: Int = rect.width * 3 / 4
        x: Int <- 0 to (img.getWidth - rect.width) by step
        y: Int <- 0 to (img.getHeight - rect.height) by step
        if x < rect.x - margin || x >= rect.x + margin || y < rect.y - margin || y >= rect.y + margin
      } {
        val imgOutFile = new File(outDir, s"left${x}_top${y}.jpg")
        ImageIO.write(img.getSubimage(x, y, rect.width, rect.height), "jpeg", imgOutFile)
        bgFileWriter.println(imgOutFile.getAbsolutePath)
      }
    } finally {
      bgFileWriter.close()
    }
    val numNeg: Int = Source.fromFile(bgFile).getLines().length

    val modelName = s"model_${objectSizePx}x${objectSizePx}_run${System.currentTimeMillis}"
    val modelDir = new File(trainingDir, modelName)
    new File(modelDir, "data").mkdirs()

    val vecCmd =
      "opencv_createsamples " +
      s"-vec ${new File(modelDir, "positive.vec").getAbsolutePath} " +
      s"-info info.dat " +
      s"-w ${objectSizePx} -h ${objectSizePx}"
    val vecLogFile = new File(modelDir, "opencv_createsamples.log")
    (s"echo Command: ${vecCmd}" #>> vecLogFile).!
    val vecLog = new PrintWriter(new FileWriter(vecLogFile, /*append=*/true))
    try {
      Process(vecCmd, trainingDir).!(ProcessLogger { line: String => vecLog.println(line) })
    } finally {
      vecLog.close()
    }

    val trainCmd =
      "opencv_traincascade " +
      s"-data ${new File(modelName, "data").getPath} " +
      s"-vec ${new File(modelName, "positive.vec").getPath} " +
      "-bg bg.txt " +
      s"-numPos ${numPos} -numNeg ${numNeg} " +
      s"-w ${objectSizePx} -h ${objectSizePx}"
    val trainLogFile = new File(modelDir, "opencv_traincascade.log")
    (s"echo Command: ${trainCmd}" #>> trainLogFile).!
    val trainLog = new PrintWriter(new FileWriter(trainLogFile, /*append=*/true))
    try {
      Process(trainCmd, trainingDir).!(ProcessLogger { line: String => trainLog.println(line) })
    } finally {
      trainLog.close()
    }
  }
}
