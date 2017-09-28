/*=========================================================================

    Copyright © 2017 BIREME/PAHO/WHO

    This file is part of MarkAbstract.

    MarkAbstract is free software: you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 2.1 of
    the License, or (at your option) any later version.

    MarkAbstract is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with MarkAbstract. If not, see <http://www.gnu.org/licenses/>.

=========================================================================*/

package org.bireme.ma

import java.io.{BufferedWriter, File, IOException}
import java.nio.charset.Charset
import java.nio.file.Files

import scala.io.Source

object MarkAbstract extends App {
  private def usage(): Unit = {
    Console.err.println("usage: MarkAbstract <inDir> <xmlFileRegexp> <outDir>")
    System.exit(1)
  }

  if (args.size != 3) usage()

  val regexHeader = "<\\?xml version=\"1..\" encoding=\"([^\"]+)\"\\?>".r
  val regex = "(\\s*)<field name=\"(ab[^\"]{0,20})\">([^<]*?)</field>".r

  // Only letters capital or lower, with and without accents, spaces and ( ) & /
  val regex3 = "(?<=(^|\\.)\\s*)[a-zA-Z][^\\u0000-\\u001f\\u0021-\\u0025\\u0027\\u002a-\\u002e\\u0030-\\u0040\\u005b-\\u005e\\u007b-\\u00bf]{0,30}\\:".r

  processFiles(args(0), args(1), args(2))

  def processFiles(inDir: String,
                   xmlRegExp: String,
                   outDir: String): Unit = {
    require (inDir != null)
    require (xmlRegExp != null)
    require (outDir != null)

    (new File(inDir)).listFiles().filter(_.isFile()).foreach {
      file => if (file.getName() matches xmlRegExp) processFile(file, outDir)
    }
  }

  private def processFile(file: File,
                          outDir: String): Unit = {
    require (file != null)
    require (outDir != null)

    print(s"Processing file: ${file.getName()} ")

    val encoding = getFileEncoding(file)
    val src = Source.fromFile(file, encoding)
    val dest = Files.newBufferedWriter((new File(outDir, file.getName)).toPath(),
                                       Charset.forName(encoding))

    processOtherFields(src.getLines, dest)

    src.close()
    dest.close()

    println("- OK")
  }

  private def getFileEncoding(file: File): String = {
    def getFileEncoding(lines: Iterator[String]): String = {
      require (lines != null)

      if (lines.hasNext) {
        val line = lines.next.trim
        if (line.isEmpty) getFileEncoding(lines)
        else {
          line match {
            case regexHeader(encoding) => encoding
            case _ => "iso-8859-1"
          }
        }
      } else "iso-8859-1"
    }

    val src = Source.fromFile(file, "iso-8859-1")
    val encoding = getFileEncoding(src.getLines)

    src.close()
    encoding
  }

  private def processOtherFields(lines: Iterator[String],
                                 dest: BufferedWriter): Unit = {
    require (lines != null)
    require (dest != null)

    while (lines.hasNext) {
      val line = lines.next.trim
      if (line.startsWith("<field name=\"ab")) processAbField(line, lines, dest)
      else dest.write(s"$line\n")
    }
  }

  private def processAbField(openLine: String,
                             lines: Iterator[String],
                             dest: BufferedWriter): Unit = {
    require (openLine != null)
    require (lines != null)
    require (dest != null)

    val field = getAbField(openLine, lines)
    dest.write(s"$field\n")

    field match {
      case regex(prefix, tag, content) =>
        val marked = splitAbstract(content).foldLeft[String]("") {
          case (str,kv) =>
            if (kv._1.isEmpty) s"$str ${kv._2}"
            else s"$str <h2>${kv._1.toUpperCase}</h2>:${kv._2}"
        }

        dest.write(s"""${prefix}<field name="${tag}_mark">${marked}</field>\n""")
    }
  }

  private def getAbField(openLine: String,
                         lines: Iterator[String]): String = {
    require (openLine != null)
    require (lines != null)

    openLine + (
      if (openLine.contains("</field>")) ""
      else if (lines.hasNext) getAbField(lines.next, lines)
      else throw new IOException("</field> expected")
    )
  }

  private def splitAbstract(abs: String): Seq[(String,String)] = {
    require(abs != null)

    val minTags = 3
    val abst = abs.trim
    val matchers = regex3.findAllMatchIn(abst).toSeq

    if ((matchers.size < minTags) ||
        (matchers.map(_.toString).toSet.size < minTags)) Seq(("", abs))
    else {
      val lastIdx = matchers.size - 1

      matchers.zipWithIndex.foldLeft[Seq[(String,String)]] (Seq()) {
        case (seq, (matcher, idx)) =>
          val auxSeq = if ((idx == 0) && (matcher.start > 0))
          seq :+ ("", abs.substring(0, matcher.start).trim) else seq

          val end = if (idx < lastIdx) matchers(idx + 1).start else abst.size
          val pos = abst.indexOf(":", matcher.start)
          auxSeq :+ (abst.substring(matcher.start, pos).trim,
                    abst.substring(pos + 1, end).trim)
      }
    }
  }
}
