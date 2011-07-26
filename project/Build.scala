import sbt._
import Keys._

object ScalaBuild extends Build {
  // lazy val projects  = Seq(root, compQuick, libQuick)
  lazy val root      = Project("scala", file(".")) aggregate(compQuick)

  // --------------------------------------------------------------
  //  Libraries used by Scalac that change infrequently
  //  (or hopefully so).
  // --------------------------------------------------------------


  // Jline nested project.   Compile this sucker once and be done.
  lazy val jline = Project("jline", file("src/jline"))
  // Fast Java Bytecode Generator (nested in every scala-compiler.jar)
  lazy val fjbg = Project("fjbg", file("src/fjbg"),
                          settings = Defaults.defaultSettings ++ Seq(
                            javaSource in Compile := file("src/fjbg"),
                            javacOptions ++= Seq("-target", "1.5")))

  // MSIL code generator
  // TODO - This probably needs to compile against quick, but Sabbus
  // had it building against locker, so we'll do worse and build
  // build against STARR for now.
  lazy val msil = Project("msil", file("src/msil"),
                          settings = Defaults.defaultSettings ++ Seq(
                            javaSource in Compile := file("src/msil"),
                            scalaSource in Compile := file("src/msil"),
                            defaultExcludes ~= (_ || "tests"),
                            javacOptions ++= Seq("-target", "1.5",
                                                 "-source", "1.4")))

  // --------------------------------------------------------------
  //  The magic kingdom.
  //  Layered compilation of Scala.
  //   Stable Reference -> Locker ('Lockable' dev version) -> Quick -> Strap (Binary compatibility testing)
  // --------------------------------------------------------------

  

	lazy val compQuick = compiler(libQuick, "quick")
	lazy val libQuick  = library("quick") settings (
	  classpathOptions := ClasspathOptions.boot
    // classpathOptions := ClasspathOptions(bootLibrary = true, compiler = false, extra = false, autoBoot = true, filterLibrary = true)
	)

	def compiler(lib: ProjectReference, stage: String): Project =
		Project("compiler-" + stage, file(".")) dependsOn(lib) aggregate(lib) settings(compilerSettings(stage) : _*)

	def library(stage: String): Project =
		Project("library-" + stage, file(".")) settings(librarySettings(stage) : _*)

	def commonSettings(stage: String, component: String): Seq[Setting[_]] = Seq(
		target <<= target(_ / stage / component),
		crossPaths := false,
		src(component),
		classpathOptions := ClasspathOptions.manual,
		sourceDirectory in Compile <<= baseDirectory(_ / "src"),
		version := "2.10.1-SNAPSHOT",
		scalaVersion := "2.9.0-1"
	)
	def librarySettings(stage: String) = commonSettings(stage, "library") ++ Seq(
		unmanaged(lib => Seq(lib / "fjbg.jar", lib / "jline.jar"))
	)
	def compilerSettings(stage: String) = commonSettings(stage, "compiler") ++ Seq(
		unmanaged(lib => Seq(lib / "fjbg.jar", lib / "jline.jar", lib /"ant" / "ant.jar", lib / "msil.jar")),
		mainClass in (Compile,run) := Some("scala.tools.nsc.MainGenericRunner")
	)

	def src(name: String) =
		scalaSource in Compile <<= sourceDirectory(_ / name)

	def unmanaged(files: File => Seq[File]) =
		unmanagedClasspath in Compile <<= unmanagedBase map { base => files(base) map Attributed.blank }
}
