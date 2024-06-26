/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.io.{BufferedWriter, File, IOException}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, StandardCopyOption}
import java.text.Normalizer
import java.text.Normalizer.Form
import java.util.{Calendar, GregorianCalendar, TimeZone}
import org.bireme.dh.{Config, Highlighter}

import scala.collection.immutable.HashSet
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
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
    Console.err.println("\t\t<xmlFileRegexp> - regular expression to filter input files")
    Console.err.println("\t\t<outDir> - the directory into where the output files will be written")
    Console.err.println("\t\t[-days=<days>] - if present only marks the files that were")
    Console.err.println("\t\tchanged in the last <days>. If absent it will mark all filtered files")
    Console.err.println("\t\t[-deCSPath=<path>] - Path to the Decs' Lucene index. if present, mark DeCS descriptors and synonyms in the abstract")
    Console.err.println("\t\t[--useTempDirs] - if present will use temporary directories for Docker performance")
    System.exit(1)
  }

  if (args.length < 4) usage()

  private val parameters: Map[String, String] = args.drop(4).foldLeft[Map[String, String]](Map()) {
    case (map, par) =>
      val split: Array[String] = par.split(" *= *", 2)
      if (split.length == 1) map + ((split(0).substring(2), ""))
      else map + ((split(0).substring(1), split(1)))
  }
  private val prefixFile: String = args(0)
  private val inDir: String = args(1)
  private val xmlFileRegexp: String = args(2)
  private val outDir: String = args(3)

  private val days: Option[Int] = parameters.get("days").map(_.toInt)
  private val deCSPath: Option[String] = parameters.get("deCSPath")
  private val useTempDirs: Boolean = parameters.contains("useTempDirs")

  private val mabs: MarkAbstract = new MarkAbstract(prefixFile, deCSPath)
  private val startTime: Long = new GregorianCalendar().getTimeInMillis
  private val result: Try[Unit] = process(inDir, xmlFileRegexp, outDir, useTempDirs, days, mabs)

  private val endTime: Long = new GregorianCalendar().getTimeInMillis
  private val difTime: Long = (endTime - startTime) / 1000
  println("\nElapsed time: " + difTime + "s")

  result match {
    case Success(_) => System.exit(0)
    case Failure(exception) =>
      exception.printStackTrace()
      System.exit(1)
  }

  private def createTmpDir(prefix: String): Try[Path] = {
    Try (Files.createTempDirectory(new File(".", "tmp").toPath, prefix))
  }
  private def delTmpDir(tmpDir: Path): Try[Unit] = {
    Try {
      val delFile: File = tmpDir.toFile

      delFile.listFiles().foreach {
        dfile =>
          if (dfile.isDirectory) delTmpDir(dfile.toPath)
          else dfile.delete()
      }
      Files.delete(delFile.toPath)
    }
  }
  private def copyFilesToDir(sourceDir: Path,
                             filter: Option[String],
                             destinationDir: Path): Try[Unit] = {
    Try {
      val sDir: File = sourceDir.toFile
      val dDir: File = destinationDir.toFile

      require(sDir.exists())
      require(dDir.exists())

      filter match {
        case Some(flt) =>
          sDir.listFiles().foreach {
            file =>
              if (file.getName.matches(flt)) {
                Files.copy(file.toPath, new File(dDir, file.getName).toPath, StandardCopyOption.REPLACE_EXISTING)
              }
          }
        case None =>
          sDir.listFiles().foreach {
            file => Files.copy(file.toPath, new File(dDir, file.getName).toPath, StandardCopyOption.REPLACE_EXISTING)
          }
      }
    }
  }

  private def process(inDir: String,
                      xmlFileRegexp: String,
                      outDir: String,
                      useTempDirs: Boolean,
                      days: Option[Int],
                      mabs: MarkAbstract): Try[Unit] = {
    if (useTempDirs) {
      for (
        inTmpDir <- createTmpDir("in");
        outTmpDir <- createTmpDir("out");
        _ <- copyFilesToDir(new File(inDir).toPath, Some(xmlFileRegexp), inTmpDir);
        _ <- mabs.processFiles(inTmpDir.toString, xmlFileRegexp, outTmpDir.toString, days);
        _ <- copyFilesToDir(outTmpDir, None, new File(outDir).toPath);
        _ <- delTmpDir(inTmpDir);
        _ <- delTmpDir(outTmpDir)
      ) yield Success[Unit](())
    } else mabs.processFiles(inDir, xmlFileRegexp, outDir, days)
  }
}

/**
  * Take a list of xml documents and add to them the the field <mark_ab> or
  * * <mark_ab_*> where every occurrence of the text xxx: yyy of the field <ab> or
  * * <ab_*> is replaced by <h2>xxx</h2>: yyy. Objective:xxx Conclusions:yyy
  *
  * @param prefixFile file having some words allowed in the abstract tag. For ex, 'Results':
  * @param deCSPath path to the DeCS lucene index path
  */
class MarkAbstract(prefixFile: String,
                   deCSPath: Option[String]) {

  private  val highlighter: Option[Highlighter] = deCSPath.map(new Highlighter(_))

  private val regex: Regex = "(\\s*)<field name=\"(ab[^\"]{0,20})\">([^<]*?)</field>".r

  private val htmlFmtSet: Set[String] = Set(
      "acronym", "abbr", "address", "b", "bdi", "bdo", "big", "blockquote",
      "center", "cite", "code", "del", "dfn", "em", "font", "i",
      "ins", "kbd", "mark", "meter", "pre", "progress", "q", "rp",
      "rt", "ruby", "s", "samp", "small", "strike", "strong", "sub",
      "sup", "time", "tt", "u", "var", "wbr"
    )

  private val prefixes: Set[String] = loadAcceptedWords(prefixFile)

  /**
    * Loads a set of words to identify which elements will be tagged with <h2> from
    * a file
    *
    * @param prefixFile - name of the file having the accepted words
    * @return the accepted words into a set
    */
  private def loadAcceptedWords(prefixFile: String): Set[String] = {
    require (prefixFile != null)

    val src: BufferedSource = Source.fromFile(prefixFile, "utf-8")

    val res: Set[String] = src.getLines().foldLeft[Set[String]](HashSet()) {
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
    * @return       Success or Failure
    */
  def processFiles(inDir: String,
                   xmlRegExp: String,
                   outDir: String,
                   days: Option[Int]): Try[Unit] = {
    require (inDir != null)
    require (xmlRegExp != null)
    require (outDir != null)
    require (days != null)

    Try[Array[File]] {
      val inDirectory: File = new File(inDir)
      assert (inDirectory.isDirectory, s"${inDirectory.getAbsolutePath} is not a directory")

      val files: Array[File] = new File(inDir).listFiles().filter(_.isFile()).filter(_.getName.matches(xmlRegExp))
      days match {
        case Some(ds) => filterFileByModDate(files, ds)
        case None => files
      }
    }.flatMap {
      files2 =>
        val checkXml = new CheckXml()
        if (files2.length == 1) {
          val file: File = files2.head
          checkXml.check(file.getPath).flatMap(_ => processFile(file, outDir))
        } else {
          files2.foreach {
            file =>
              checkXml.check(file.getPath).flatMap(_ => processFile(file, outDir)) match {
                case Success(_) => ()
                case Failure(exception) => println(s"Skipping file [${file.getName}] - ${exception.toString}")
              }
          }
          Success(())
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
    * @return Success or Failure
    */
  private def processFile(file: File,
                          outDir: String): Try[Unit] = {
    require (file != null)
    require (outDir != null)

    Try {
      val fname: String = file.getName
      println(s"\nProcessing file: $fname")

      val encoding = getFileEncoding(file)
      val src = Source.fromFile(file, encoding)
      val dir = new File(outDir)
      if (!dir.exists) dir.mkdir()
      val dest = Files.newBufferedWriter(
        new File(dir, file.getName).toPath, Charset.forName(encoding))

      processOtherFields(fname, src.getLines(), dest)

      src.close()
      dest.close()

      println("\nOK")
    }
  }

  /**
    * Given a xml, figure out its encoding according to its header.
    *
    * @param file input xml file
    * @return the xml encoding
    */
  private def getFileEncoding(file: File): String = {
    @scala.annotation.tailrec
    def getFileEncoding1(lines: Iterator[String]): String = {
      require (lines != null)

      if (lines.hasNext) {
        val line = lines.next().trim
        if (line.isEmpty) getFileEncoding1(lines)
        else {
          val regexHeader = "<\\?xml +version=[\"']1..[\"'] +encoding=[\"']([^\"]+)[\"']\\?>".r

          line match {
            case regexHeader(encod) => encod
            case _ => "iso-8859-1"
          }
        }
      } else "iso-8859-1"
    }

    val src = Source.fromFile(file, "iso-8859-1")
    val encoding = getFileEncoding1(src.getLines())

    src.close()
    encoding
  }

  /**
    * Copy the xml field into the destination file if it is not of kind abstract,
    * if it is, then call the function that will mark and save it.
    *
    * @param fname the processed file name
    * @param lines the lines of the input xml file
    * @param dest  the output xml file
    */
  private def processOtherFields(fname: String,
                                 lines: Iterator[String],
                                 dest: BufferedWriter): Unit = {
    require (lines != null)
    require (dest != null)

    val regex1: Regex =  "<field name=\"la\">([^<]{2})</field>".r
    val regexAb: Regex = "<field name=\"ab_([a-z]{2})\">".r
    var cur: Int = 0
    var lang: Option[String] = None

    while (lines.hasNext) {
      val line = lines.next().trim
      if (lang.isEmpty && line.startsWith("<field name=\"la")) {
        lang = regex1.findFirstMatchIn(line).map(mat => mat.group(1).toLowerCase)
      } else if (line.startsWith("<field name=\"ab")) {
        val lang1: Option[String] = regexAb.findFirstMatchIn(line).map(mat => mat.group(1).toLowerCase).
          orElse(lang).orElse(Some("en"))
        if (cur % 10000 == 0) println(s"+++$cur")
        cur += 1
        processAbField(fname, lang1, line, lines, dest)
      } else {
        dest.write(line)
        dest.newLine()
      }
    }
  }

  /**
    * Mark with <h2> the abstract tag and the DeCS terms saving them into the output xml file
    *
    * @param fname the processed file name
    * @param lang abstract text language
    * @param openLine the line having the open tag <ab> or <ab_*>
    * @param lines the lines of the input xml file
    * @param dest  the output xml file
    */
  private def processAbField(fname: String,
                             lang: Option[String],
                             openLine: String,
                             lines: Iterator[String],
                             dest: BufferedWriter): Unit = {
    require (fname != null)
    require (openLine != null)
    require (lines != null)
    require (dest != null)

    val field: String = getAbField(openLine, lines)

    dest.write(field)
    dest.newLine()

    field match {
      case regex(prefix, tag, content) =>
        val noFmtContent: String = removeFmtHtmlMarks(content) // remove html formatting tags
        val marked: String = markSentence(noFmtContent, prefixes, lang)

        dest.write(prefix + "<field name=\"mark_" + tag + "\">" + marked + "</field>\n")
      case _ => ()
    }
  }

  /**
    * Given a sentence mark with <h2> if it contains abstracts parts (OBJECTIVES, METHODS, CONCLUSIONS) and also
    * mark its DeCS terms
    * @param sentence input sentence to be marked
    * @param prefixes a set of keywords of abstract parts (OBJECTIVES, METHODS, CONCLUSIONS, etc)
    * @param lang the language of the sentence
    * @return the sentence marked
    */
  private def markSentence(sentence: String,
                           prefixes: Set[String],
                           lang: Option[String]): String = {
    if (prefixes.isEmpty) markDeCSDescriptors(sentence, lang)
    else sentence.split(" *:").map {
      split =>
        val (str: String, seqPos: Seq[Int]) = org.bireme.dh.Tools.uniformString2(split)
        prefixes.find(str.endsWith) match {
          case Some(prefix) =>
            //val begPos: Int = seqPos(split.length - prefix.length)
            val begPos: Int = seqPos(seqPos.length - prefix.length)
            val decsMarked: String = markDeCSDescriptors(split.substring(0, begPos), lang)
            decsMarked + "&lt;h2&gt;" + split.substring(begPos) + ":&lt;/h2&gt;"
          case None => markDeCSDescriptors(split, lang)
        }
    }.mkString("")
  }

  /**
    * Given an input text, mark all DeCS descriptors and synonyms
    * @param text input text
    * @param lang abstract text language
    * @return the input text marked
    */
  private def markDeCSDescriptors(text: String,
                                  lang: Option[String]): String = {
    if (highlighter.nonEmpty) {
      Try {
        val suffix = "&lt;/a&gt;"
        val scanLang: Option[String] = lang.flatMap(str => Some(str).filter(Set("en", "es", "pt", "fr").contains))
        //val conf: Config = Config(scanLang, None, None, scanDescriptors = true, scanSynonyms = true, onlyPreCod = false)
        val conf: Config = Config(scanLang, outLang = None, scanMainHeadings = true, scanEntryTerms = true,
                                  scanQualifiers = true, scanPublicationTypes = true, scanCheckTags= true,
                                  scanGeographics = true)
        val (_, seq, _) = highlighter.get.highlight("", "", text, conf)
        val (marked: String, tend: Int) = seq.foldLeft[(String, Int)]("", 0) {
          case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, id: String, _, _, _)) =>
            val prefix = s"""&lt;a class="decs" id="$id"&gt;"""
            //val prefix = s"""<a class="decs" id="$id">"""
            val s = str + text.substring(lpos, termBegin) + prefix + text.substring(termBegin, termEnd + 1) + suffix
            (s, termEnd + 1)
        }
        if (tend >= text.length) marked else marked + text.substring(tend)
      } match {
        case Success(v) => v
        case Failure(_) => text
      }
    } else text
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
      else if (lines.hasNext) getAbField(lines.next(), lines)
      else throw new IOException("</field> expected")
      )
  }

  /**
    * Converts all input charactes into a-z, 0-9, '-' and spaces
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
    val regex1 = "&lt;/?\\s*([^\\s>]+?)\\s*&gt;".r

    def replace(mat: Match): Option[String] =
      if (htmlFmtSet.contains(mat.group(1).toLowerCase)) Some("")
      else None

    require (str != null)
    regex1.replaceSomeIn(str, replace)
  }
}
