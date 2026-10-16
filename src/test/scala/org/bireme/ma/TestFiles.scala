/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
  * Helpers to create input files and inspect the output of the tests
  */
object TestFiles {
  val UTF8: Charset = StandardCharsets.UTF_8
  val ISO: Charset = StandardCharsets.ISO_8859_1

  // Prefixes are stored as uniformString would leave them (lower case, no accents)
  val prefixes: Seq[String] = Seq("objetivo", "objetivos", "objective", "methods", "results", "main results",
                                  "resultados", "conclusoes", "conclusions")

  val header: String = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

  private val markFieldRegex = "(?s)<field name=\"mark_[^\"]*\">(.*?)</field>".r
  private val markLineRegex = "(?s)[ \\t]*<field name=\"mark_[^\"]*\">.*?</field>\\R"

  def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit = {
    if (Files.exists(path)) {
      Using.resource(Files.walk(path)) {
        paths => paths.sorted(Comparator.reverseOrder[Path]()).forEach(p => Files.delete(p))
      }
    }
  }

  def writePrefixFile(dir: Path): Path =
    Files.write(dir.resolve("prefixes.txt"), prefixes.mkString("\n").getBytes(UTF8))

  def write(dir: Path,
            name: String,
            content: String,
            charset: Charset = UTF8): Path = Files.write(dir.resolve(name), content.getBytes(charset))

  def writeBytes(dir: Path,
                 name: String,
                 bytes: Array[Byte]): Path = Files.write(dir.resolve(name), bytes)

  def read(path: Path,
           charset: Charset = UTF8): String = new String(Files.readAllBytes(path), charset)

  def fileNames(dir: Path): Set[String] =
    if (Files.isDirectory(dir)) Using.resource(Files.list(dir))(_.iterator().asScala.map(_.getFileName.toString).toSet)
    else Set()

  /** @return an xml file content with the given documents */
  def xml(docs: String*): String = header + "<add>\n" + docs.mkString + "</add>\n"

  /** @return a <doc> element with the given (field name, field content) pairs */
  def doc(fields: (String, String)*): String =
    "  <doc>\n" + fields.map { case (name, value) => s"""    <field name="$name">$value</field>\n""" }.mkString +
      "  </doc>\n"

  /** @return the contents of all mark fields (<field name="mark_...">) */
  def marks(output: String): Seq[String] = markFieldRegex.findAllMatchIn(output).map(_.group(1)).toSeq

  /** @return the output without the mark fields. It must be equal to the input. */
  def withoutMarks(output: String): String = output.replaceAll(markLineRegex, "")

  def count(text: String,
            sub: String): Int = {
    var total: Int = 0
    var pos: Int = text.indexOf(sub)
    while (pos >= 0) {
      total += 1
      pos = text.indexOf(sub, pos + sub.length)
    }
    total
  }

  def countFields(text: String,
                  name: String): Int = count(text, s"""<field name="$name">""")

  def h2(prefix: String): String = s"&lt;h2&gt;$prefix:&lt;/h2&gt;"

  def decs(id: String,
           term: String): String = s"""&lt;a class="decs" id="$id"&gt;$term&lt;/a&gt;"""
}
