/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.nio.file.{Files, Path, Paths}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

import TestFiles.*

/**
  * Integration tests with the DeCS Lucene index (slower). The index path is given by the system property
  * 'decs.path' or the environment variable DECS_PATH. If the index is not found, the tests are canceled.
  */
class MarkAbstractDeCSTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val decsPath: Path = Paths.get(sys.props.get("decs.path").orElse(sys.env.get("DECS_PATH"))
    .getOrElse("/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs"))

  private val suiteDir: Path = tempDir("ma-decs-suite")
  private val prefixFile: String = writePrefixFile(suiteDir).toString

  private lazy val mabs: MarkAbstract = new MarkAbstract(prefixFile, Some(decsPath.toString), 8)

  override def afterAll(): Unit = deleteRecursively(suiteDir)

  private def withDirs(test: (Path, Path) => Any): Unit = {
    assume(Files.isDirectory(decsPath), s"DeCS index not found: $decsPath")
    val base: Path = tempDir("ma-decs-test")
    try test(Files.createDirectories(base.resolve("in")), base.resolve("out"))
    finally deleteRecursively(base)
  }

  private def markOne(content: String,
                      ma: => MarkAbstract = mabs): String = {
    var output: String = ""
    withDirs { (in, out) =>
      write(in, "a.xml", content)
      ma.processFiles(in.toString, ".*\\.xml", out.toString, None) shouldBe Success(())
      output = read(out.resolve("a.xml"))
    }
    output
  }

  private val asthma = decs("d001249", "asthma")
  private val children = decs("d002648", "children")

  "MarkAbstract with DeCS" should "mark the DeCS terms together with the <h2> prefixes and colons" in {
    val output = markOne(xml(doc("la" -> "pt",
      "ab" -> "Objetivos: avaliar a razão 3:1 em http://x.org na saúde. Conclusões: houve melhora")))
    marks(output) shouldBe Seq(s"${h2("Objetivos")} avaliar a razão 3:1 em http://x.org na " +
      s"${decs("d006262", "saúde")}. ${h2("Conclusões")} houve melhora")
  }

  it should "use the language of the 'la' field even when it is after the abstract" in {
    // without the 'la' field the default language is English, and 'asthma' would be marked
    val output = markOne(xml(doc("ab" -> "Objetivo: asthma", "la" -> "pt")))
    marks(output) shouldBe Seq(s"${h2("Objetivo")} asthma")
  }

  it should "use English if the document has no language" in {
    val output = markOne(xml(doc("ab" -> "Objective: children with asthma")))
    marks(output) shouldBe Seq(s"${h2("Objective")} $children with $asthma")
  }

  it should "use the language of each document, not of the first one of the file" in {
    val output = markOne(xml(doc("la" -> "pt", "ab" -> "Objetivo: crianças"),
                             doc("la" -> "en", "ab" -> "Objective: children with asthma")))
    marks(output)(1) shouldBe s"${h2("Objective")} $children with $asthma"
  }

  it should "not mark English terms in a Portuguese abstract" in {
    val output = markOne(xml(doc("la" -> "pt", "ab" -> "Objetivo: asthma")))
    marks(output) shouldBe Seq(s"${h2("Objetivo")} asthma")
  }

  it should "use the language of the field name (ab_en) instead of the 'la' field" in {
    val output = markOne(xml(doc("la" -> "pt", "ab_en" -> "Objective: asthma")))
    output should include (s"""<field name="mark_ab_en">${h2("Objective")} $asthma</field>""")
  }

  it should "write the same output with 1 or several threads" in {
    val texts = Seq("Objective: health of children with asthma", "Objetivos: saúde da criança com asma",
                    "Results: diabetes and obesity in adolescents", "Conclusões: vacinação e tuberculose")
    val input = xml((1 to 300).map(i => doc("id" -> i.toString, "la" -> (if (i % 2 == 0) "en" else "pt"),
                                            "ab" -> texts(i % texts.size)))*)
    val output8 = markOne(input)
    val output1 = markOne(input, new MarkAbstract(prefixFile, Some(decsPath.toString), 1))

    output8 shouldBe output1
    count(output1, "class=\"decs\"") should be > 300
  }
}
