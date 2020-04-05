name := """puddingscam-opencv-model-builder"""
organization := "my.edu.clhs"
version := "1.0-SNAPSHOT"
maintainer := "Jack Leow"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.1"

resolvers += Resolver.mavenLocal

dependencyOverrides += "com.google.guava" % "guava" % "28.1-jre"

libraryDependencies += guice
libraryDependencies += "org.bytedeco" % "javacv-platform" % "1.4.3"
libraryDependencies += "org.bytedeco.javacpp-presets" % "opencv-platform" % "3.4.3-1.4.3"
libraryDependencies += "org.bytedeco.javacpp-presets" % "opencv" % "3.4.3-1.4.3" classifier "macosx-x86_64-gpu"
libraryDependencies += "org.bytedeco.javacpp-presets" % "cuda" % "10.0-7.3-1.4.3" classifier "macosx-x86_64-redist"
libraryDependencies += "org.libraw" % "jlibraw" % "0.1-SNAPSHOT"

libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0" % Runtime

javaOptions in Universal ++= Seq(
  // -J params will be added as jvm parameters
  "-J-Djava.library.path=conf/native/"
)

lazy val elmMake = taskKey[Seq[File]]("elm-make")

elmMake := {
  import scala.sys.process._
  import com.typesafe.sbt.packager.Compat.ProcessLogger
  import com.typesafe.sbt.web.LineBasedProblem
  import play.sbt.PlayExceptions.CompilationException

  val outputPath: String = "public/javascripts/puddings-cam.elm.js"
  val debugFlag: String =
    if (sys.props.getOrElse("elm.debug", "false").toLowerCase != "true") ""
    else "--debug"
  var outErrLines: List[String] = Nil
  var srcFilePath: Option[String] = None
  var lineNum: Option[String] = None
  var offset: Option[String] = None
  Seq(
    "bash", "-c",
    "elm-make " +
    (file("app/assets/javascripts") ** "*.elm").get.mkString(" ") +
    s" --output ${outputPath} " +
    s"--yes ${debugFlag} --warn"
  ).!(
    new ProcessLogger {
      override def out(s: => String): Unit = {
        streams.value.log.info(s)
        outErrLines = s :: outErrLines
      }

      override def err(s: => String): Unit = {
        streams.value.log.warn(s)
        val SrcFilePathExtractor = """-- [A-Z ]+ -+ (app/assets/javascripts/.+\.elm)""".r
        val LineNumExtractor = """([0-9]+)\|.*""".r
        val PosExtractor = """ *\^+ *""".r
        s match {
          case SrcFilePathExtractor(path: String) =>
            srcFilePath = srcFilePath orElse Some(path)
          case LineNumExtractor(num: String) =>
            lineNum = lineNum orElse Some(num)
          case PosExtractor() =>
            offset = offset orElse Some(s)
          case _ =>
        }
        outErrLines = s :: outErrLines
      }

      override def buffer[T](f: => T): T = f
    }
  ) match {
    case 0 =>
      streams.value.log.success("elm-make completed.")
      Seq(file(outputPath), file("elm-stuff"))

    case 127 =>
      streams.value.log.warn("elm-make not found in PATH. Skipping Elm build.")
      Nil

    case _ =>
      throw CompilationException(
        new LineBasedProblem(
          message = outErrLines.reverse.mkString("\n"),
          severity = null,
          lineNumber = lineNum.map(_.toInt).getOrElse(0),
          characterOffset = offset.map(_.indexOf('^') - 2 - lineNum.map(_.length).getOrElse(0)).getOrElse(0),
          lineContent = "",
          source = file(srcFilePath.getOrElse("app/assets/javascripts/Main.elm"))
        )
      )
  }
}

sourceGenerators in Assets += elmMake.taskValue

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "my.edu.clhs.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "my.edu.clhs.binders._"
