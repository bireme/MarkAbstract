lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "2.0.0",
  scalaVersion := "2.13.10"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "MarkAbstract"
  )

//val servletApiVersion = "4.0.1"
val jakartaServletApiVersion = "6.0.0"
val luceneVersion = "9.4.2" //"8.11.1" //"9.1.0" //"8.8.2" //"8.6.3"
val scalaTestVersion = "3.3.0-SNAP3" //"3.3.0-SNAP2"

libraryDependencies ++= Seq(
  //"javax.servlet" % "javax.servlet-api" % servletApiVersion % "provided",
  "jakarta.servlet" % "jakarta.servlet-api" % jakartaServletApiVersion % "provided",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  //"org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  //"org.apache.lucene" % "lucene-analysis-common" % luceneVersion,
  "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

Test / logBuffered := false
trapExit :=  false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

enablePlugins(JettyPlugin)

containerPort := 7272

assembly / assemblyMergeStrategy  := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}
