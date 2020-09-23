lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "2.0.0",
  scalaVersion := "2.13.3"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "MarkAbstract"
  )

val servletApiVersion = "4.0.1"
val luceneVersion = "8.6.2"
val scalaTestVersion = "3.3.0-SNAP2" //"3.2.0-M1"

libraryDependencies ++= Seq(
  "javax.servlet" % "javax.servlet-api" % servletApiVersion % "provided",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

logBuffered in Test := false
trapExit :=  false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

enablePlugins(JettyPlugin)

containerPort := 7272