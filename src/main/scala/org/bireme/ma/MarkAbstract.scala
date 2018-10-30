/*=========================================================================

    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.ma

import java.io.{BufferedWriter, File, IOException}
import java.nio.charset.Charset
import java.nio.file.Files
import java.text.Normalizer
import java.text.Normalizer.Form
import java.util.{Calendar, GregorianCalendar, TimeZone}

import org.bireme.dh.{CharSeq, Highlighter, Tools}

import scala.io.Source
import scala.util.matching.Regex.Match

/**
  * Take a list of xml documents and add to them the the field <mark_ab> or
  * <mark_ab_*> where every occurrence of the text xxx: yyy of the field <ab> or
  * <ab_*> is replaced by <h2>xxx</h2>: yyy. Objective:xxx Conclusions:yyy
  */
object MarkAbstract extends App {
  private def usage(): Unit = {
    Console.err.println("usage: MarkAbstract")
    Console.err.println("\t\t<prefixFile> - file having some words allowed in the abstract tag. For ex, 'Results':")
    Console.err.println("\t\t<inDir> - directory having the input files used to created the marked ones")
    Console.err.println("\t\t<xmlFileRegexp> - regular expression to filter input files that follow the pattern <word>..<word>:")
    Console.err.println("\t\t<outDir> - the directory into where the output files will be written")
    Console.err.println("\t\t[-days=<days>] - if present only marks the files that are")
    Console.err.println("\t\tchanged in the last <days>. If absent marks all filtered files")
    Console.err.println("\t\t[-deCSPath=<path>] - Path to the Decs' Isis database. if present, mark DeCS descriptors and synonyms in the abstract")
    System.exit(1)
  }

  if (args.length < 4) usage()

  val parameters = args.drop(4).foldLeft[Map[String,String]](Map()) {
    case (map,par) =>
      val split = par.split(" *= *", 2)
      if (split.size == 1) map + ((split(0).substring(2), ""))
      else map + ((split(0).substring(1), split(1)))
  }

  val days = parameters.get("days").map(_.toInt)
  val deCSPath = parameters.get("deCSPath")
  val regexHeader = "<\\?xml version=\"1..\" encoding=\"([^\"]+)\"\\?>".r
  val regex = "(\\s*)<field name=\"(ab[^\"]{0,20})\">([^<]*?)</field>".r
  val oneWordDotRegex = "(^|\\.)\\s*([^\\.\\s]+)[\\.\\:]".r

  // Only letters capital or lower, with and without accents, spaces and ( ) & /
  //val regex2 = "(?<=(^|\\.)\\s*)[a-zA-Z][^\\u0000-\\u001f\\u0021-\\u0025\\u0027\\u002a-\\u002e\\u0030-\\u0040\\u005b-\\u005e\\u007b-\\u00bf]{0,30}\\:".r
  val regex2 = "(^|\\.)\\s*[a-zA-Z][^\\u0000-\\u001f\\u0021-\\u0025\\u0027\\u002a-\\u002e\\u0030-\\u0040\\u005b-\\u005e\\u007b-\\u00bf]{0,30}\\:".r

  val regex3 = "\\&lt;/?\\s*([^\\s\\>]+?)\\s*\\&gt;".r
  val htmlFmtSet = Set(
    "acronym", "abbr", "address", "b", "bdi", "bdo", "big", "blockquote",
    "center", "cite", "code", "del", "dfn", "em", "font", "i",
    "ins", "kbd", "mark", "meter", "pre", "progress", "q", "rp",
    "rt", "ruby", "s", "samp", "small", "strike", "strong", "sub",
    "sup", "time", "tt", "u", "var", "wbr"
  )

  val wordDotSet = Set (
    "conclusion", "conclusiones", "conclusions", "conclusao", "conclusoes",
    "method", "methods", "metodology", "metodo", "metodos", "metodologia",
    "objective", "objectives", "objetivo", "objetivos", "objectivos",
    "result", "results", "resultado", "resultados"
    )

  val prefixes = loadAcceptedWords(args(0))

  val highlighter = new Highlighter()
  val terms: Map[String,String] = if (deCSPath.nonEmpty) Tools.decs2Set(deCSPath.get) else Map[String,String]()
  val tree: Map[Char, CharSeq] = if (deCSPath.nonEmpty) highlighter.createTermTree(terms) else Map[Char, CharSeq]()

  processFiles(args(1), args(2), args(3), days)

  /**
    * Loads a set of words to identy which elements will bt tagged with <h2> from
    * a file
    *
    * @param prefixFile - name of the file having the accepted words
    * @return the accepted words into a set
    */
  private def loadAcceptedWords(prefixFile: String): Set[String] = {
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

  /**
    * Mark the elements of the abstract with tag <h2>
    *
    * @param inDir - input xml files directory
    * @param xmlRegExp - regular expression used to filter input xml files
    * @param outDir - output directory where the marked xml files will be created
    * @param days - number the days from today used to filter modified xml files.
    *               If None then all xml files will be processed regardless the
    *               modification date.
    */
  def processFiles(inDir: String,
                   xmlRegExp: String,
                   outDir: String,
                   days: Option[Int]): Unit = {
    require (inDir != null)
    require (xmlRegExp != null)
    require (outDir != null)
    require (days != null)

    val checkXml = new CheckXml()
    val files = new File(inDir).listFiles().filter(_.isFile())
    val files2 = days match {
      case Some(ds) => filterFileByModDate(files, ds)
      case None => files
    }
    files2.foreach { file =>
      val fname = file.getName
      if (fname matches xmlRegExp) {
        checkXml.check(file.getPath) match {
          case Some(errMess) => println(s"Skipping file [$fname] - $errMess")
          case None => processFile(file, outDir)
        }
      }
    }
  }

  /**
    * Given a list of files, filter those whose modification date are younger
    * than x days.
    *
    * @param files list of input files to be filtered
    * @param days number of days used to filter the list of files
    * @return a list of files filtered by modification date
    */
  private def filterFileByModDate(files: Array[File],
                                  days: Int): Array[File] = {
    require (files != null)

    val now = new GregorianCalendar(TimeZone.getDefault)
    val year = now.get(Calendar.YEAR)
    val month = now.get(Calendar.MONTH)
    val day = now.get(Calendar.DAY_OF_MONTH)
    val todayCal = new GregorianCalendar(year, month, day, 0, 0) // begin of today
    val beforeCal = todayCal.clone().asInstanceOf[GregorianCalendar]
    beforeCal.add(Calendar.DAY_OF_MONTH, -1 * days)   // begin of date
    val before = beforeCal.getTime.getTime        // begin of before date

    files.filter(_.lastModified >= before)
  }

  /**
    * Given a xml file, marks all abstract elements according to a set of
    * of accepted words, and then write a copy of this files into an output diret
    *
    * @param file input xml file
    * @param outDir output directory of xml files
    */
  private def processFile(file: File,
                          outDir: String): Unit = {
    require (file != null)
    require (outDir != null)

    print(s"Processing file: ${ file.getName} ")

    val encoding = getFileEncoding(file)
    val src = Source.fromFile(file, encoding)
    val dir = new File(outDir)
    if (!dir.exists) dir.mkdir()
    val dest = Files.newBufferedWriter(
      new File(dir, file.getName).toPath, Charset.forName(encoding))

    processOtherFields(src.getLines, dest)

    src.close()
    dest.close()

    println("- OK")
  }

  /**
    * Given a xml, figure out its encoding according to its header.
    *
    * @param file input xml file
    * @return the xml encoding
    */
  private def getFileEncoding(file: File): String = {
    def getFileEncoding(lines: Iterator[String]): String = {
      require (lines != null)

      if (lines.hasNext) {
        val line = lines.next.trim
        if (line.isEmpty) getFileEncoding(lines)
        else {
          line match {
            case regexHeader(encod) => encod
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

  /**
    * Copy the xml field into the destination file if it is not of kind abstract,
    * if it is, then call the function that will mark and save it.
    *
    * @param lines the lines of the input xml file
    * @param dest  the output xml file
    */
  private def processOtherFields(lines: Iterator[String],
                                 dest: BufferedWriter): Unit = {
    require (lines != null)
    require (dest != null)

    while (lines.hasNext) {
      val line = lines.next.trim
      if (line.startsWith("<field name=\"ab"))
        processAbField(line, lines, dest)
      else dest.write(line + "\n")
    }
  }

  /**
    * Mark with <h2> the abstract tag and save it into the output xml file
    *
    * @param openLine the line having the open tag <ab> or <ab_*>
    * @param lines the lines of the input xml file
    * @param dest  the output xml file
    */
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
        val noFmtContent = removeFmtHtmlMarks(content) // remove html formatting tags
        val marked = processWordColonAbField(noFmtContent) getOrElse
          (processOneWordDotAbField(noFmtContent) getOrElse noFmtContent)
        val marked2 = markDeCSDescriptors(marked)

        // <span class="decs" id=""> </span>
        dest.write(prefix + "<field name=\"mark_" + tag + "\">" + marked2 +
                                                                   "</field>\n")
      case _ => ()
    }
  }

  /**
    * Given an input text, mark all DeCS descriptors and dynonyms
    * @param text input text
    * @return the input text marked
    */
  private def markDeCSDescriptors(text: String): String = {
    if (deCSPath.nonEmpty) {
      val suffix = "&lt;/span&gt;"
      val (_, seq, _) = highlighter.highlight("", "", text, tree)
      val (marked: String, tend: Int) = seq.foldLeft("", 0) {
        case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, id: String)) =>
          val prefix = s"""&lt;span class="decs" id="$id"&gt;"""
          val s = str + text.substring(lpos, termBegin) + prefix + text.substring(termBegin, termEnd + 1) + suffix
          (s, termEnd + 1)
      }
      if (tend >= text.length) marked else marked + text.substring(tend)
    } else text
  }

  /**
    * Mark with <h2> the abstract tag and save it into the output xml file.
    * Looks for '. <word> <word> :' pattern
    *
    * @param content abstract field content
    * @return the marked content
    */
  private def processWordColonAbField(content: String): Option[String] = {
    require (content != null)

    val (out,marked) =
      splitAbstract(content).foldLeft[(String,Boolean)](("", false)) {
        case ((str,found),kv) =>
          if (kv._1.isEmpty) (str + kv._2, found)
          else if (shouldMark(kv._1))
            (str + "&lt;h2&gt;" + kv._1.toUpperCase + ":&lt;/h2&gt; " + kv._2, true)
          else (str + kv._1 + ": " + kv._2, found)
      }
    if (marked) Some(out) else None
  }

  /**
    * Mark with <h2> the abstract tag and save it into the output xml file
    * Looks for '. <word>.' or '. <word>:' pattern.
    *
    * @param content abstract field content
    * @return the marked content
    */
  private def processOneWordDotAbField(content: String): Option[String] = {
    require (content != null)

    val matchers = oneWordDotRegex findAllMatchIn content
     markMatchers(content, 0, matchers, "", marked = false)
  }

  /**
    * Given a list of matcher mark each match if it is present at a given
    * authorized list
    *
    * @param text input text to be marked
    * @param initPos initial position of the text to start marking (used by recursion)
    * @param matchers the list of pattern matchers
    * @param prefix the already marked text (used by recursion)
    * @param marked if true indicates that the prefix is already marked
    * @return the initial text marked with <h2>
    */
  private def markMatchers(text: String,
                           initPos: Int,
                           matchers: Iterator[Match],
                           prefix: String,
                           marked: Boolean): Option[String] = {
    require (text != null)
    require (initPos >= 0)
    require (matchers != null)
    require(prefix != null)

    if (matchers.hasNext) {
      val mat = matchers.next
      val word = mat.group(2)

      if (wordDotSet.contains(uniformString(word))) {
        val newPrefix = prefix + text.substring(initPos, mat.start(2)) +
          "&lt;h2&gt;" + word.toUpperCase + ":&lt;/h2&gt;"
        markMatchers(text, mat.end, matchers, newPrefix, marked = true)
      } else markMatchers(text, initPos, matchers, prefix, marked)
    } else {
      if (marked) Some(prefix + text.substring(initPos))
      else None
    }
  }

  /**
    * Retrieve the abstract field <ab>xxx</ab> from an input String
    *
    * @param openLine the line having the open tag <ab> or <ab_*>
    * @param lines the lines of the input xml file
    * @return the string with the abstract xml element
    */
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

 /**
   * Given an input string, returns a sequence of prefix and suffix of substrings
   * of type xxx:yyy as Conclusions:bla bla.
   *
   * @param abs input string to be parsed
   * @return sequence of pairs of prefix and suffix of the parsed substrings
   */
  private def splitAbstract(abs: String): Seq[(String,String)] = {
    require(abs != null)

    val minTags = 3 // Requiring at least minTags expressions of type XXXXX: to consider then as a markable tag
    val matchers = regex2.findAllMatchIn(abs).toSeq
    //println(s"matchers size=${matchers.size}")
    if (matchers.map(_.toString).toSet.size < minTags) Seq(("", abs))
    else {
      val lastIdx = matchers.size - 1

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
          } else abs.length
          val pos = abs.indexOf(":", start)

          auxSeq :+ (abs.substring(start, pos).trim,
                    abs.substring(pos + 1, end).trim)
      }
    }
  }

  /**
    * Given an input string, tell if it should be marked with <h2> or not according
    * to the accepted words set
    *
    * @param in input string
    * @return true if the string should be marked and false if not
    */
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

    s2.replaceAll("[^\\w\\-]", " ").trim  // Hifen
  }

  /**
    * Remove formatting html marks from a string
    *
    * @param str input String
    * @return the input string with html marks removed
    */
    private def removeFmtHtmlMarks(str: String): String = {
      def replace(mat: Match): Option[String] =
        if (htmlFmtSet.contains(mat.group(1).toLowerCase)) Some("") else None

      require (str != null)
      regex3.replaceSomeIn(str, replace)
    }

  /**
    * Figure out the number of caracteres to jump before to reach a letter
    *
    * @param matcher the piece of text matched by the regular expression
    * @return the number of caracteres to jump before to reach a letter
    */
  private def shift(matcher: Match): Int = matcher.toString.indexWhere(
                                               ch => (ch >= 'A') && (ch <= 'z'))
}
