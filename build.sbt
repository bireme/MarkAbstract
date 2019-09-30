lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "0.1.0",
  scalaVersion := "2.13.1"  //"2.13.0"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "MarkAbstract"
  )

val scalaTestVersion = "3.2.0-M1" //"3.0.8"
//val hairyfotrVersion = "0.1.17"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

logBuffered in Test := false
trapExit :=  false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)
