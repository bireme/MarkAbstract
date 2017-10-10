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
import java.text.Normalizer
import java.text.Normalizer.Form
import java.util.{Calendar,GregorianCalendar,TimeZone}

import scala.io.Source
import scala.util.matching.Regex.Match

object MarkAbstract extends App {
  private def usage(): Unit = {
    Console.err.println("usage: MarkAbstract")
    Console.err.println("\t\t<prefixFile> - file having some words allowed in the abstract tag. For ex, 'Results':")
    Console.err.println("\t\t<inDir> - directory having the input files used to created the marked ones")
    Console.err.println("\t\t<xmlFileRegexp> - regular expression to filter input files")
    Console.err.println("\t\t<outDir> - the directory into where the output files will be written")
    Console.err.println("\t\t[<days>] - if present only marks the files that are")
    Console.err.println("\t\tchanged in the last <days>. If absent marks all filtered files")
    System.exit(1)
  }

  if (args.size < 4) usage()

  val days = if (args.size > 4) Some(args(4).toInt) else None
  val regexHeader = "<\\?xml version=\"1..\" encoding=\"([^\"]+)\"\\?>".r
  val regex = "(\\s*)<field name=\"(ab[^\"]{0,20})\">([^<]*?)</field>".r

  // Only letters capital or lower, with and without accents, spaces and ( ) & /
  //val regex2 = "(?<=(^|\\.)\\s*)[a-zA-Z][^\\u0000-\\u001f\\u0021-\\u0025\\u0027\\u002a-\\u002e\\u0030-\\u0040\\u005b-\\u005e\\u007b-\\u00bf]{0,30}\\:".r
  val regex2 = "(^|\\.)\\s*[a-zA-Z][^\\u0000-\\u001f\\u0021-\\u0025\\u0027\\u002a-\\u002e\\u0030-\\u0040\\u005b-\\u005e\\u007b-\\u00bf]{0,30}\\:".r

  val prefixes = loadPrefixes(args(0))

  processFiles(args(1), args(2), args(3), days)

  private def loadPrefixes(prefixFile: String): Set[String] = {
    require (prefixFile != null)

    val src = Source.fromFile(prefixFile, "utf-8")

    val res = src.getLines.foldLeft[Set[String]](Set()) {
      case (set,line) =>
        val lineT = line.trim
        if (lineT.isEmpty) set else set + uniformString(lineT)
    }

    src.close()
    res
  }

  def processFiles(inDir: String,
                   xmlRegExp: String,
                   outDir: String,
                   days: Option[Int]): Unit = {
    require (inDir != null)
    require (xmlRegExp != null)
    require (outDir != null)
    require (days != null)

    val files = new File(inDir).listFiles().filter(_.isFile())
    val files2 = days match {
      case Some(ds) => filterFileByModDate(files, ds)
      case None => files
    }
    files2.foreach(
      file => if (file.getName() matches xmlRegExp) processFile(file, outDir)
    )
  }

  private def filterFileByModDate(files: Array[File],
                                  days: Int): Array[File] = {
    require (files != null)

    val now = new GregorianCalendar(TimeZone.getDefault())
    val year = now.get(Calendar.YEAR)
    val month = now.get(Calendar.MONTH)
    val day = now.get(Calendar.DAY_OF_MONTH)
    val todayCal = new GregorianCalendar(year, month, day, 0, 0) // begin of today
    val beforeCal = todayCal.clone().asInstanceOf[GregorianCalendar]
    beforeCal.add(Calendar.DAY_OF_MONTH, -1 * days)   // begin of date
    val before = beforeCal.getTime().getTime()        // begin of before date

    files.filter(_.lastModified >= before)
  }

  private def processFile(file: File,
                          outDir: String): Unit = {
    require (file != null)
    require (outDir != null)

    print(s"Processing file: ${file.getName()} ")

    val encoding = getFileEncoding(file)
    val src = Source.fromFile(file, encoding)
    val dir = new File(outDir)
    if (!dir.exists) dir.mkdir()
    val dest = Files.newBufferedWriter((new File(dir, file.getName)).toPath(),
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
      else dest.write(line + "\n")
    }
  }

  private def processAbField(openLine: String,
                             lines: Iterator[String],
                             dest: BufferedWriter): Unit = {
    require (openLine != null)
    require (lines != null)
    require (dest != null)

    val field = getAbField(openLine, lines)
    dest.write(field + "\n")

    field match {
      case regex(prefix, tag, content) =>
        val marked = splitAbstract(content).foldLeft[String]("") {
          case (str,kv) =>
            if (kv._1.isEmpty) str + kv._2
            else if (shouldMark(kv._1))
              str + "<h2>" + kv._1.toUpperCase + "</h2>: " + kv._2
            else
              str + kv._1 + ": " + kv._2
        }

        dest.write(prefix + "<field name=\"" + tag + "_mark\">" + marked +
                                                                   "</field>\n")
      case _ => ()
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
    val matchers = regex2.findAllMatchIn(abs).toSeq

    if (matchers.map(_.toString).toSet.size < minTags) Seq(("", abs))
    else {
      val lastIdx = matchers.size - 1
/*
      matchers.map(_.toString.toLowerCase).foreach {
        mt => {
          if (!apagar.contains(mt)) {
            apagar += mt
            println(mt)
          }
        }
      }
*/
      matchers.zipWithIndex.foldLeft[Seq[(String,String)]] (Seq()) {
        case (seq, (matcher, idx)) =>
          val start = matcher.start + shift(matcher)
          val auxSeq = if ((idx == 0) && (matcher.start > 0)) {
            val startMat = if (abs(start) == '.') start + 1 else start
            seq :+ ("", abs.substring(0, startMat))
          } else seq

          val end = if (idx < lastIdx) {
            val nextMatPos = matchers(idx + 1).start
            if (abs(nextMatPos) == '.') nextMatPos + 1 else nextMatPos
          } else abs.size
          val pos = abs.indexOf(":", start)

          auxSeq :+ (abs.substring(start, pos).trim,
                    abs.substring(pos + 1, end).trim)
      }
    }
  }

  private def shouldMark(in: String): Boolean =
    uniformString(in).split("\\s+").exists(word => prefixes.contains(word))

  /**
    * Converts all input charactes into a-z, 0-9, '_', '-' and spaces
    *
    * @param in input string to be converted
    * @return the converted string
    */
  private def uniformString(in: String): String = {
    require (in != null)

    val s1 = Normalizer.normalize(in.trim().toLowerCase(), Form.NFD)
    val s2 = s1.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")

    //s2.replaceAll("\\W", " ")
    s2.replaceAll("[^\\w\\-]", " ").trim  // Hifen
  }

  private def shift(matcher: Match): Int = matcher.toString.indexWhere(
                                               ch => (ch >= 'A') && (ch <= 'z'))
}
