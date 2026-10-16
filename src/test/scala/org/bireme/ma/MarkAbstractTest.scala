/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.io.IOException
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant
import java.time.temporal.ChronoUnit
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xml.sax.SAXParseException

import scala.util.{Failure, Random, Success, Try}

import TestFiles.*

/**
  * Tests of MarkAbstract without the DeCS index (fast). They cover the <h2> marking, encodings,
  * error handling, memory limits, parallelism and command line parameters.
  */
class MarkAbstractTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val suiteDir: Path = tempDir("ma-suite")
  private val prefixFile: String = writePrefixFile(suiteDir).toString

  private def newMarkAbstract(threads: Int = 4): MarkAbstract = new MarkAbstract(prefixFile, None, threads)

  private lazy val mabs: MarkAbstract = newMarkAbstract()

  override def afterAll(): Unit = deleteRecursively(suiteDir)

  /** Runs the test with a new input directory and an (not yet created) output directory */
  private def withDirs(test: (Path, Path) => Any): Unit = {
    val base: Path = tempDir("ma-test")
    try test(Files.createDirectories(base.resolve("in")), base.resolve("out"))
    finally deleteRecursively(base)
  }

  private def run(in: Path,
                  out: Path,
                  ma: MarkAbstract = mabs,
                  regex: String = ".*\\.xml",
                  days: Option[Int] = None,
                  checkXml: Boolean = false): Try[Unit] =
    ma.processFiles(in.toString, regex, out.toString, days, checkXml)

  /** Marks a single file and returns its output */
  private def markOne(content: String): String = {
    var output: String = ""
    withDirs { (in, out) =>
      write(in, "a.xml", content)
      run(in, out) shouldBe Success(())
      output = read(out.resolve("a.xml"))
    }
    output
  }

  // ---------------------------------------------------------------- marking

  "MarkAbstract" should "mark the abstract parts with <h2>" in {
    val input = xml(doc("id" -> "1", "ab" -> "Objetivos: avaliar. Conclusões: houve melhora"))
    marks(markOne(input)) shouldBe Seq(s"${h2("Objetivos")} avaliar. ${h2("Conclusões")} houve melhora")
  }

  it should "keep the colons that do not follow a prefix (ex: 3:1, http://)" in {
    val input = xml(doc("ab" -> "Objetivos: razão 3:1 em http://x.org"))
    marks(markOne(input)) shouldBe Seq(s"${h2("Objetivos")} razão 3:1 em http://x.org")
  }

  it should "mark the longest prefix" in {
    val input = xml(doc("ab" -> "Main results: positive"))
    marks(markOne(input)) shouldBe Seq(s"${h2("Main results")} positive")
  }

  it should "copy an abstract without prefixes" in {
    val input = xml(doc("ab" -> "Just a text: without prefix"))
    marks(markOne(input)) shouldBe Seq("Just a text: without prefix")
  }

  it should "keep the line breaks of an abstract with several lines" in {
    val input = header + "<add>\n  <doc>\n    <field name=\"ab\">Objective: evaluate the health\n" +
      "of children</field>\n  </doc>\n</add>\n"
    val output = markOne(input)

    marks(output) shouldBe Seq(s"${h2("Objective")} evaluate the health\nof children")
    withoutMarks(output) shouldBe input
  }

  it should "remove html formatting tags only from the mark field" in {
    val input = xml(doc("ab" -> "Results: &lt;i&gt;asthma&lt;/i&gt; and &lt;h1&gt;x&lt;/h1&gt;"))
    val output = markOne(input)

    marks(output) shouldBe Seq(s"${h2("Results")} asthma and &lt;h1&gt;x&lt;/h1&gt;")
    withoutMarks(output) shouldBe input
  }

  it should "create the mark field of an abstract with language (ab_pt -> mark_ab_pt)" in {
    val output = markOne(xml(doc("la" -> "en", "ab_pt" -> "Objetivo: x")))
    output should include (s"""    <field name="mark_ab_pt">${h2("Objetivo")} x</field>""")
  }

  it should "keep all the other fields, the 'la' field and the indentation" in {
    val input = xml(doc("id" -> "1", "la" -> "pt", "ti" -> "Title", "ab" -> "Results: x", "au" -> "Author"),
                    doc("id" -> "2", "ab" -> "Results: y"))
    val output = markOne(input)

    withoutMarks(output) shouldBe input
    countFields(output, "mark_ab") shouldBe 2
  }

  it should "write the mark field before a </doc> that is in the same line of the abstract" in {
    val input = header + "<add>\n  <doc>\n    <field name=\"ab\">Results: x</field></doc>\n</add>\n"
    markOne(input) shouldBe header + "<add>\n  <doc>\n    <field name=\"ab\">Results: x</field>\n" +
      s"    <field name=\"mark_ab\">${h2("Results")} x</field>\n</doc>\n</add>\n"
  }

  it should "not create the mark field of an abstract in an unexpected format (CDATA)" in {
    val input = xml(doc("ab" -> "<![CDATA[Results: x < y]]>"))
    markOne(input) shouldBe input
  }

  // -------------------------------------------------------------- encodings

  it should "read and write an iso-8859-1 file without xml header" in {
    withDirs { (in, out) =>
      write(in, "a.xml", "<add>\n  <doc>\n    <field name=\"ab\">Objetivo: saúde</field>\n  </doc>\n</add>\n", ISO)
      run(in, out) shouldBe Success(())

      val bytes = Files.readAllBytes(out.resolve("a.xml"))
      bytes should contain (0xFA.toByte)                       // 'ú' in iso-8859-1
      marks(new String(bytes, ISO)) shouldBe Seq(s"${h2("Objetivo")} saúde")
    }
  }

  it should "read and write an utf-8 file without xml header" in {
    withDirs { (in, out) =>
      write(in, "a.xml", "<add>\n  <doc>\n    <field name=\"ab\">Objetivo: saúde</field>\n  </doc>\n</add>\n", UTF8)
      run(in, out) shouldBe Success(())
      marks(read(out.resolve("a.xml"), UTF8)) shouldBe Seq(s"${h2("Objetivo")} saúde")
    }
  }

  it should "accept an xml header with BOM and standalone attribute" in {
    withDirs { (in, out) =>
      val content = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<add>\n" +
        "  <doc>\n    <field name=\"ab\">Objetivo: saúde</field>\n  </doc>\n</add>\n"
      writeBytes(in, "a.xml", Array(0xEF.toByte, 0xBB.toByte, 0xBF.toByte) ++ content.getBytes(UTF8))
      run(in, out) shouldBe Success(())

      val bytes = Files.readAllBytes(out.resolve("a.xml"))
      bytes.take(3) shouldBe Array(0xEF.toByte, 0xBB.toByte, 0xBF.toByte)
      marks(new String(bytes, UTF8)) shouldBe Seq(s"${h2("Objetivo")} saúde")
    }
  }

  it should "accept an xml header in the same line of the root element" in {
    withDirs { (in, out) =>
      write(in, "a.xml", "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><add>\n  <doc>\n" +
        "    <field name=\"ab\">Objetivo: saúde</field>\n  </doc>\n</add>\n", ISO)
      run(in, out) shouldBe Success(())
      marks(read(out.resolve("a.xml"), ISO)) shouldBe Seq(s"${h2("Objetivo")} saúde")
    }
  }

  it should "replace invalid bytes instead of skipping the file" in {
    withDirs { (in, out) =>
      val content = header.getBytes(UTF8) ++ "<add>\n  <doc>\n    <field name=\"ab\">Results: bad ".getBytes(UTF8) ++
        Array(0xFF.toByte) ++ " byte</field>\n  </doc>\n</add>\n".getBytes(UTF8)
      writeBytes(in, "a.xml", content)
      run(in, out) shouldBe Success(())
      marks(read(out.resolve("a.xml"))) shouldBe Seq(s"${h2("Results")} bad � byte")
    }
  }

  // ------------------------------------------------------- files and errors

  it should "fail an abstract without </field>, leaving no output file" in {
    withDirs { (in, out) =>
      write(in, "a.xml", header + "<add>\n  <doc>\n    <field name=\"ab\">Objective: x\n")
      val result = run(in, out)

      result.isFailure shouldBe true
      result.failed.get shouldBe an [IOException]
      result.failed.get.getMessage shouldBe "</field> expected"
      fileNames(out) shouldBe empty                            // neither a.xml nor a.xml.tmp
    }
  }

  it should "process the other files when one of them fails, and then report the failure" in {
    withDirs { (in, out) =>
      write(in, "a.xml", xml(doc("ab" -> "Results: a")))
      write(in, "b.xml", xml(doc("ab" -> "Results: b")))
      write(in, "c.xml", header + "<add>\n  <doc>\n    <field name=\"ab\">Objective: x\n")
      val result = run(in, out)

      result.failed.get.getMessage shouldBe "1 of 3 file(s) failed: c.xml"
      result.failed.get.getSuppressed.map(_.getMessage).toSeq shouldBe Seq("</field> expected")
      fileNames(out) shouldBe Set("a.xml", "b.xml")
    }
  }

  it should "process only the files that match the regular expression" in {
    withDirs { (in, out) =>
      write(in, "a.xml", xml(doc("ab" -> "Results: a")))
      write(in, "b.txt", xml(doc("ab" -> "Results: b")))
      run(in, out) shouldBe Success(())
      fileNames(out) shouldBe Set("a.xml")
    }
  }

  it should "process only the files modified in the last days" in {
    withDirs { (in, out) =>
      write(in, "new.xml", xml(doc("ab" -> "Results: a")))
      val old = write(in, "old.xml", xml(doc("ab" -> "Results: b")))
      Files.setLastModifiedTime(old, FileTime.from(Instant.now().minus(10, ChronoUnit.DAYS)))

      run(in, out, days = Some(1)) shouldBe Success(())
      fileNames(out) shouldBe Set("new.xml")
    }
  }

  it should "create the output directory and its parents" in {
    withDirs { (in, out) =>
      write(in, "a.xml", xml(doc("ab" -> "Results: a")))
      run(in, out.resolve("x/y")) shouldBe Success(())
      fileNames(out.resolve("x/y")) shouldBe Set("a.xml")
    }
  }

  it should "fail if the input directory does not exist" in {
    withDirs { (in, out) =>
      run(in.resolve("missing"), out).isFailure shouldBe true
    }
  }

  it should "check the xml before marking it if checkXml is true" in {
    withDirs { (in, out) =>
      write(in, "a.xml", "<add><doc>\n<field name=\"ab\">Results: x</field>\n</add>\n")   // <doc> not closed

      val checked = run(in, out, checkXml = true)
      checked.failed.get shouldBe a [SAXParseException]
      fileNames(out) shouldBe empty

      run(in, out, checkXml = false) shouldBe Success(())
      fileNames(out) shouldBe Set("a.xml")
    }
  }

  // ---------------------------------------------------------- memory limits

  private val filler: String = "x" * 1000
  private val bigDocLines: Int = 11000            // 11000 lines x 1000 chars > 10 MB (maxDocChars)

  it should "process a document bigger than the memory limit" in {
    val builder = new StringBuilder(header + "<add>\n")
    (1 to 100).foreach(i => builder.append(doc("id" -> i.toString, "ab" -> s"Results: before $i")))
    builder.append("  <doc>\n    <field name=\"la\">en</field>\n")
    (1 to bigDocLines).foreach(_ => builder.append(s"    <field name=\"ft\">$filler</field>\n"))
    builder.append("    <field name=\"ab\">Results: big</field>\n  </doc>\n")
    (1 to 100).foreach(i => builder.append(doc("id" -> i.toString, "ab" -> s"Results: after $i")))
    builder.append("</add>\n")
    val input = builder.toString

    withDirs { (in, out) =>
      write(in, "a.xml", input)
      run(in, out, ma = newMarkAbstract(1)) shouldBe Success(())
      val output1 = read(out.resolve("a.xml"))

      withoutMarks(output1) shouldBe input
      countFields(output1, "mark_ab") shouldBe 201
      output1 should include (s"""<field name="mark_ab">${h2("Results")} big</field>""")

      // the big document is a barrier in the parallel flow: the result must be the same
      run(in, out, ma = newMarkAbstract(8)) shouldBe Success(())
      read(out.resolve("a.xml")) shouldBe output1
    }
  }

  it should "process a file whose documents have no </doc>" in {
    val builder = new StringBuilder(header + "<add>\n")
    (1 to 6000).foreach {
      i => builder.append(s"  <doc>\n    <field name=\"ft\">$filler$filler</field>\n" +
                          s"    <field name=\"ab\">Results: $i</field>\n  </doc >\n")
    }
    builder.append("</add>\n")
    val input = builder.toString

    withDirs { (in, out) =>
      write(in, "a.xml", input)
      run(in, out) shouldBe Success(())
      val output = read(out.resolve("a.xml"))

      withoutMarks(output) shouldBe input
      countFields(output, "mark_ab") shouldBe 6000
    }
  }

  it should "fail an abstract bigger than the memory limit without </field>" in {
    val builder = new StringBuilder(header + "<add>\n  <doc>\n    <field name=\"ab\">Results: start\n")
    (1 to bigDocLines).foreach(_ => builder.append(filler).append('\n'))
    builder.append("</add>\n")

    withDirs { (in, out) =>
      write(in, "a.xml", builder.toString)
      val result = run(in, out)

      result.failed.get shouldBe an [IOException]
      result.failed.get.getMessage should include ("bigger than")
      fileNames(out) shouldBe empty
    }
  }

  // ------------------------------------------------------------ parallelism

  private def randomDocs(total: Int): String = {
    val random = new Random(42)
    val texts = Seq("Objetivos: avaliar a razão 3:1. Conclusões: melhora", "Main results: positive",
                    "Methods: cross-sectional\nstudy with adults", "Just text without prefix",
                    "Results: &lt;i&gt;asthma&lt;/i&gt; in children")
    val docs = (1 to total).map {
      i =>
        val abField = if (random.nextInt(4) == 0) "ab_pt" else "ab"
        doc(Seq("id" -> i.toString, "la" -> Seq("en", "pt", "es")(random.nextInt(3)),
                abField -> texts(random.nextInt(texts.size))) ++
            (if (random.nextBoolean()) Seq("ti" -> s"Title $i") else Seq())*)
    }
    xml(docs*)
  }

  it should "write the same output with 1 or several threads" in {
    withDirs { (in, out) =>
      write(in, "a.xml", randomDocs(3000))

      run(in, out.resolve("t1"), ma = newMarkAbstract(1)) shouldBe Success(())
      run(in, out.resolve("t8"), ma = newMarkAbstract(8)) shouldBe Success(())

      val output1 = read(out.resolve("t1/a.xml"))
      read(out.resolve("t8/a.xml")) shouldBe output1
      countFields(output1, "mark_ab") + countFields(output1, "mark_ab_pt") shouldBe 3000
    }
  }

  it should "report the original exception of an error inside the parallel flow" in {
    val docs = (1 to 1000).map {
      i => if (i == 500) "  <doc>\n    <field name=\"ab\">Results: no end\n  </doc>\n"
           else doc("id" -> i.toString, "ab" -> s"Results: $i")
    }
    withDirs { (in, out) =>
      write(in, "a.xml", xml(docs*))
      val result = run(in, out, ma = newMarkAbstract(8))

      result.failed.get shouldBe an [IOException]          // not the Ox ChannelClosedException
      result.failed.get.getMessage shouldBe "</field> expected"
      fileNames(out) shouldBe empty
    }
  }

  it should "not accept a number of threads lesser than 1" in {
    an [IllegalArgumentException] should be thrownBy new MarkAbstract(prefixFile, None, 0)
  }

  // ----------------------------------------------- command line and tmp dirs

  "MarkAbstract.parseParameters" should "accept parameters with '-' or '--'" in {
    MarkAbstract.parseParameters(Seq("-days=3", "--useTempDirs", "-checkXml", "--deCSPath=/a/b", "-threads = 2")) shouldBe
      Map("days" -> "3", "useTempDirs" -> "", "checkXml" -> "", "deCSPath" -> "/a/b", "threads" -> "2")
  }

  "MarkAbstract.defaultThreads" should "be the number of processors - 1, at least 1" in {
    MarkAbstract.defaultThreads shouldBe math.max(1, Runtime.getRuntime.availableProcessors() - 1)
  }

  "MarkAbstract.process" should "give the same result with temporary directories and delete them" in {
    withDirs { (in, out) =>
      write(in, "a.xml", randomDocs(50))
      val tmpBase = out.resolve("tmp")

      MarkAbstract.process(in.toString, ".*\\.xml", out.resolve("direct").toString, useTempDirs = false,
                           None, checkXml = false, mabs) shouldBe Success(())
      MarkAbstract.process(in.toString, ".*\\.xml", out.resolve("viaTmp").toString, useTempDirs = true,
                           None, checkXml = false, mabs, tmpBase) shouldBe Success(())

      read(out.resolve("viaTmp/a.xml")) shouldBe read(out.resolve("direct/a.xml"))
      fileNames(tmpBase) shouldBe empty
    }
  }

  it should "copy the marked files and delete the temporary directories when a file fails" in {
    withDirs { (in, out) =>
      write(in, "a.xml", xml(doc("ab" -> "Results: a")))
      write(in, "c.xml", header + "<add>\n  <doc>\n    <field name=\"ab\">Objective: x\n")
      val tmpBase = out.resolve("tmp")

      val result = MarkAbstract.process(in.toString, ".*\\.xml", out.resolve("o").toString, useTempDirs = true,
                                        None, checkXml = false, mabs, tmpBase)
      result.isFailure shouldBe true
      fileNames(out.resolve("o")) shouldBe Set("a.xml")
      fileNames(tmpBase) shouldBe empty
    }
  }
}
