lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "2.0.0",
  scalaVersion := "3.3.7" //"2.13.16"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "MarkAbstract"
  )

//val servletApiVersion = "4.0.1"
val jakartaServletApiVersion = "6.1.0"
val luceneVersion = "10.3.2" //"9.12.1" //"9.5.0" //"8.11.1" //"9.1.0" //"8.8.2" //"8.6.3"
val scalaTestVersion = "3.3.0-SNAP4" //"3.3.0-SNAP2"

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

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

//enablePlugins(JettyPlugin)
enablePlugins(SbtWar)

//containerPort := 7272

/*assembly / assemblyMergeStrategy  := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}*/

assembly / assemblyMergeStrategy  := {
  case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}

// Força a "major version" da compilaçao ser 69 (java 25)
ThisBuild / scalacOptions ++= Seq("-release", "25")
Compile / javacOptions ++= Seq("--release", "25") // só se você tiver fontes Java


