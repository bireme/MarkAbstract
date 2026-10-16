/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.io.{BufferedWriter, File, IOException, InputStreamReader, OutputStreamWriter, StringWriter, Writer}
import java.nio.charset.{Charset, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.text.Normalizer
import java.text.Normalizer.Form
import java.util.{Calendar, Comparator, GregorianCalendar, TimeZone}
import org.bireme.dh.{Config, Highlighter}
import ox.channels.ChannelClosedException
import ox.flow.Flow

import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Codec, Source}
import scala.util.{Failure, Success, Try, Using}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
  * Take a list of xml documents and add to them the field <mark_ab> or
  * <mark_ab_*> where every occurrence of the text xxx: yyy of the field <ab> or
  * <ab_*> is replaced by <h2>xxx</h2>: yyy. Objective:xxx Conclusions:yyy
  */
object MarkAbstract {
  /** Number of processors - 1 (leaves one processor to read and write the files), at least 1 */
  def defaultThreads: Int = math.max(1, Runtime.getRuntime.availableProcessors() - 1)

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
    Console.err.println("\t\t[--checkXml] - if present checks if each input file is a well formed xml before marking it")
    Console.err.println("\t\t[-threads=<n>] - number of documents marked in parallel. Default: number of processors - 1")
    System.exit(1)
  }

  /**
    * @param args the optional command line parameters (after the 4 mandatory ones), ex: -days=3, --useTempDirs
    * @return map of parameter name (without '-' or '--') -> value ("" if it has no value)
    */
  private[ma] def parseParameters(args: Seq[String]): Map[String, String] = {
    args.foldLeft[Map[String, String]](Map()) {
      case (map, par) =>
        val split = par.dropWhile(_ == '-').split(" *= *", 2)
        map + ((split(0), if (split.length == 1) "" else split(1)))
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 4) usage()

    val parameters: Map[String, String] = parseParameters(args.toSeq.drop(4))
    val prefixFile: String = args(0)
    val inDir: String = args(1)
    val xmlFileRegexp: String = args(2)
    val outDir: String = args(3)

    val days: Option[Int] = parameters.get("days").map(_.toInt)
    val deCSPath: Option[String] = parameters.get("deCSPath")
    val useTempDirs: Boolean = parameters.contains("useTempDirs")
    val checkXml: Boolean = parameters.contains("checkXml")
    val threads: Int = parameters.get("threads").map(_.toInt).getOrElse(defaultThreads)

    val mabs: MarkAbstract = new MarkAbstract(prefixFile, deCSPath, threads)
    val startTime: Long = new GregorianCalendar().getTimeInMillis
    val result: Try[Unit] = process(inDir, xmlFileRegexp, outDir, useTempDirs, days, checkXml, mabs)

    val endTime: Long = new GregorianCalendar().getTimeInMillis
    val difTime: Long = (endTime - startTime) / 1000
    println("\nElapsed time: " + difTime + "s")

    result match {
      case Success(_) => System.exit(0)
      case Failure(exception) =>
        exception.printStackTrace()
        System.exit(1)
    }
  }

  private def createTmpDir(tmpBase: Path,
                           prefix: String): Try[Path] = {
    Try (Files.createTempDirectory(Files.createDirectories(tmpBase), prefix))
  }
  private def delTmpDir(tmpDir: Path): Try[Unit] = {
    Try {
      Using.resource(Files.walk(tmpDir)) {
        paths => paths.sorted(Comparator.reverseOrder[Path]()).forEach(path => Files.delete(path)) // children first
      }
    }
  }

  /**
    * Deletes a temporary directory after the operation that used it, whatever its result
    * @return the operation result or, if it succeeded, the deletion result
    */
  private def deleteAfter(result: Try[Unit],
                          tmpDir: Path): Try[Unit] = {
    val deleted: Try[Unit] = delTmpDir(tmpDir)
    deleted.failed.foreach(exception => println(s"Unable to delete temporary directory [$tmpDir] - $exception"))
    result.flatMap(_ => deleted)
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

  private[ma] def process(inDir: String,
                          xmlFileRegexp: String,
                          outDir: String,
                          useTempDirs: Boolean,
                          days: Option[Int],
                          checkXml: Boolean,
                          mabs: MarkAbstract,
                          tmpBase: Path = Paths.get("tmp")): Try[Unit] = {
    if (useTempDirs) {
      createTmpDir(tmpBase, "in").flatMap {
        inTmpDir =>
          val result: Try[Unit] = createTmpDir(tmpBase, "out").flatMap {
            outTmpDir =>
              val res: Try[Unit] = copyFilesToDir(Paths.get(inDir), Some(xmlFileRegexp), inTmpDir).flatMap {
                _ =>
                  val processed: Try[Unit] =
                    mabs.processFiles(inTmpDir.toString, xmlFileRegexp, outTmpDir.toString, days, checkXml)
                  // copies the files that were marked even if some other file failed
                  val copied: Try[Unit] = Try(Files.createDirectories(Paths.get(outDir)))
                    .flatMap(dir => copyFilesToDir(outTmpDir, None, dir))
                  processed.flatMap(_ => copied)
              }
              deleteAfter(res, outTmpDir)
          }
          deleteAfter(result, inTmpDir)
      }
    } else mabs.processFiles(inDir, xmlFileRegexp, outDir, days, checkXml)
  }
}

/**
  * Take a list of xml documents and add to them the field <mark_ab> or
  * * <mark_ab_*> where every occurrence of the text xxx: yyy of the field <ab> or
  * * <ab_*> is replaced by <h2>xxx</h2>: yyy. Objective:xxx Conclusions:yyy
  *
  * @param prefixFile file having some words allowed in the abstract tag. For ex, 'Results':
  * @param deCSPath path to the DeCS lucene index path
  * @param threads number of documents marked in parallel. Default: number of processors - 1 (at least 1)
  */
class MarkAbstract(prefixFile: String,
                   deCSPath: Option[String],
                   threads: Int = MarkAbstract.defaultThreads) {
  require (threads > 0, s"threads must be positive: $threads")

  private  val highlighter: Option[Highlighter] = deCSPath.map(new Highlighter(_))

  private val regex: Regex = "(\\s*)<field name=\"(ab[^\"]{0,20})\">([^<]*?)</field>".r
  private val laRegex: Regex = "<field name=\"la\">([^<]{2})</field>".r
  private val abLangRegex: Regex = "<field name=\"ab_([a-z]{2})\">".r
  private val colonRegex: Regex = " *:".r
  private val headerRegex: Regex =
    "^<\\?xml\\s+version\\s*=\\s*[\"']1\\.\\d[\"']\\s+encoding\\s*=\\s*[\"']([^\"']+)[\"']".r
  private val utf8BomAsIso: String = "\u00EF\u00BB\u00BF"   // UTF-8 BOM bytes read as iso-8859-1
  private val fmtTagRegex: Regex = "&lt;/?\\s*([^\\s>]+?)\\s*&gt;".r

  // DeCS highlighter configurations, created once for each scanned language (None = all languages)
  private val scanLangs: Set[String] = Set("en", "es", "pt", "fr")
  private val decsConfigs: Map[Option[String], Config] =
    (None +: scanLangs.toSeq.map(Some(_))).map {
      scanLang => scanLang -> Config(scanLang, outLang = None, scanMainHeadings = true, scanEntryTerms = true,
                                     scanQualifiers = true, scanPublicationTypes = true, scanCheckTags = true,
                                     scanGeographics = true)
    }.toMap

  // Limits of what is kept in memory from a <doc> or from an abstract field. They protect against
  // huge documents or files without the closing tags, which would otherwise be read entirely.
  private val maxDocChars: Long = 10L * 1024 * 1024
  private val maxDocLines: Int = 100000
  private val maxFieldChars: Long = 10L * 1024 * 1024

  // Documents are marked in parallel in batches of about this size (chars). Small batches keep all
  // threads busy even in small files; bigger ones would leave threads idle at the end of the file.
  private val batchChars: Long = 64L * 1024
  private val lineSeparator: String = System.lineSeparator()

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
    * @param checkXml - if true, checks if each input file is a well formed xml before marking it
    * @return       Success or Failure
    */
  def processFiles(inDir: String,
                   xmlRegExp: String,
                   outDir: String,
                   days: Option[Int],
                   checkXml: Boolean = false): Try[Unit] = {
    require (inDir != null)
    require (xmlRegExp != null)
    require (outDir != null)
    require (days != null)

    Try[Array[File]] {
      val files: Array[File] = new File(inDir).listFiles().filter(_.isFile()).filter(_.getName.matches(xmlRegExp))
      days match {
        case Some(ds) => filterFileByModDate(files, ds)
        case None => files
      }
    }.flatMap {
      files2 =>
        val checker: CheckXml = new CheckXml()
        val failures: Seq[(String, Throwable)] = files2.toSeq.flatMap {
          file =>
            val checked: Try[Unit] = if (checkXml) checker.check(file.getPath).map(_ => ()) else Success(())
            checked.flatMap(_ => processFile(file, outDir)) match {
              case Success(_) => None
              case Failure(exception) =>
                println(s"Skipping file [${file.getName}] - ${exception.toString}")
                Some((file.getName, exception))
            }
        }
        failures match {
          case Seq() => Success(())
          case Seq((_, exception)) if files2.length == 1 => Failure(exception)
          case _ =>
            val exception = new IOException(s"${failures.size} of ${files2.length} file(s) failed: " +
              failures.map(_._1).mkString(", "))
            failures.foreach(fail => exception.addSuppressed(fail._2))
            Failure(exception)
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

    val fname: String = file.getName
    println(s"\nProcessing file: $fname")

    // Writes into a temporary file and only renames it at the end, so a failure never leaves
    // a truncated xml with the final name in the output directory
    Try {
      val dir: Path = Files.createDirectories(Paths.get(outDir))
      (dir.resolve(fname + ".tmp"), dir.resolve(fname))
    }.flatMap {
      case (tmpFile, outFile) =>
        val result: Try[Unit] = for {
          encoding <- Try(getFileEncoding(file))
          _ <- Using.Manager { use =>
            // Invalid bytes are replaced instead of making the whole file be skipped
            val codec: Codec = Codec(encoding).onMalformedInput(CodingErrorAction.REPLACE)
                                              .onUnmappableCharacter(CodingErrorAction.REPLACE)
            val src = use(Source.fromFile(file)(codec))
            val dest = use(new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(tmpFile),
              Charset.forName(encoding).newEncoder().onMalformedInput(CodingErrorAction.REPLACE)
                                                    .onUnmappableCharacter(CodingErrorAction.REPLACE))))
            processOtherFields(fname, src.getLines(), dest)
          }
          _ <- Try(Files.move(tmpFile, outFile, StandardCopyOption.REPLACE_EXISTING,
                              StandardCopyOption.ATOMIC_MOVE))
        } yield println("\nOK")

        if (result.isFailure) Try(Files.deleteIfExists(tmpFile))
        result
    }
  }

  /**
    * Given a xml, figure out its encoding according to its header. If the header does not
    * declare it, the file is checked: utf-8 (the xml default) if valid, otherwise iso-8859-1.
    *
    * @param file input xml file
    * @return the xml encoding
    */
  private def getFileEncoding(file: File): String = {
    val firstLine: Option[String] = Using.resource(Source.fromFile(file, "iso-8859-1")) {
      src => src.getLines().map(_.stripPrefix(utf8BomAsIso).trim).find(_.nonEmpty)
    }

    firstLine.flatMap(headerRegex.findFirstMatchIn).map(_.group(1)).getOrElse {
      if (isValidUtf8(file)) "utf-8" else "iso-8859-1"
    }
  }

  /**
    * @param file input file
    * @return true if all file bytes are a valid utf-8 sequence
    */
  private def isValidUtf8(file: File): Boolean = {
    Try {
      // newDecoder() reports (throws) malformed input by default
      Using.resource(new InputStreamReader(Files.newInputStream(file.toPath), StandardCharsets.UTF_8.newDecoder())) {
        reader =>
          val buffer: Array[Char] = new Array[Char](64 * 1024)
          while (reader.read(buffer) != -1) {}
      }
    }.isSuccess
  }

  /**
    * A complete <doc> (or a single line outside of a <doc>) held in memory
    * @param lines the document lines
    * @param lang the document language (field <la>)
    * @param chars the number of chars of the document
    */
  private case class Doc(lines: ArrayBuffer[String],
                         lang: Option[String],
                         chars: Long)

  /**
    * Copy the xml field into the destination file if it is not of kind abstract,
    * if it is, then call the function that will mark and save it.
    * Each <doc> is buffered so that its language (field <la>) is known before its
    * abstracts are marked, wherever the <la> field appears inside the document.
    * The buffered documents are grouped in batches that are marked in parallel (Ox flow) and
    * written in their original order.
    * If a <doc> exceeds maxDocChars or maxDocLines, the parallel flow is finished, the buffered
    * lines are processed with the language found so far, the rest of the document is processed
    * line by line and then a new parallel flow processes the following documents.
    *
    * @param fname the processed file name
    * @param lines the lines of the input xml file
    * @param dest  the output xml file
    */
  private def processOtherFields(fname: String,
                                 lines: Iterator[String],
                                 dest: Writer): Unit = {
    require (lines != null)
    require (dest != null)

    var cur: Int = 0                                // number of abstracts already written
    var oversized: Option[Doc] = None               // partial <doc> bigger than the memory limit

    def progress(abstracts: Int): Unit = {
      (cur until cur + abstracts).filter(_ % 10000 == 0).foreach(pos => println(s"+++$pos"))
      cur += abstracts
    }

    // Reads the next documents and stops before an oversized one, which is left in 'oversized'.
    // It is consumed only by the flow source, and 'oversized' is read only after the flow is finished.
    def docs: Iterator[Doc] = new Iterator[Doc] {
      private var nextDoc: Option[Doc] = None

      override def hasNext: Boolean = {
        if (nextDoc.isEmpty && oversized.isEmpty && lines.hasNext) {
          readDoc(lines) match {
            case Right(doc) => nextDoc = Some(doc)
            case Left(partial) => oversized = Some(partial)
          }
        }
        nextDoc.nonEmpty
      }

      override def next(): Doc = {
        if (!hasNext) throw new NoSuchElementException()
        val doc: Doc = nextDoc.get
        nextDoc = None
        doc
      }
    }

    var finished: Boolean = false
    while (!finished) {
      try {
        Flow.fromIterator(docs)
          .groupedWeighted(batchChars)(_.chars)
          .mapPar(threads)(batch => processBatch(fname, batch))
          .runForeach {
            case (text, abstracts) =>
              dest.write(text)
              progress(abstracts)
          }
      } catch {   // reports the original error instead of the Ox channel wrapper
        case ex: ChannelClosedException.Error if ex.getCause != null => throw ex.getCause
      }

      oversized match {
        case Some(partial) =>
          oversized = None
          println(s"WARNING: [$fname] <doc> bigger than the memory limit ($maxDocChars chars or $maxDocLines lines)" +
            s" or without </doc> - processing the rest of it line by line with language " +
            partial.lang.getOrElse("default"))
          progress(processLines(fname, partial.lines.iterator ++ untilDocEnd(lines), partial.lang, dest))
        case None => finished = true
      }
    }
  }

  /**
    * Reads the next document (lines from <doc> up to </doc>), or a single line if it is outside a document
    * @param lines the lines of the input xml file
    * @return Right(document) or Left(partial document) if it is bigger than maxDocChars or maxDocLines
    */
  private def readDoc(lines: Iterator[String]): Either[Doc, Doc] = {
    val line: String = lines.next()

    if ((startsWithIgnoringIndent(line, "<doc>") || startsWithIgnoringIndent(line, "<doc ")) &&
        !line.contains("</doc>")) {
      val docLines = ArrayBuffer[String](line)
      var docChars: Long = line.length

      while (!docLines.last.contains("</doc>") && lines.hasNext &&
             (docChars <= maxDocChars) && (docLines.length <= maxDocLines)) {
        val next: String = lines.next()
        docLines += next
        docChars += next.length
      }
      val lang: Option[String] = docLines.iterator.flatMap(laRegex.findFirstMatchIn).nextOption()
        .map(_.group(1).toLowerCase)
      val doc: Doc = Doc(docLines, lang, docChars)

      if (docLines.last.contains("</doc>") || !lines.hasNext) Right(doc) else Left(doc)
    } else Right(Doc(ArrayBuffer(line), None, line.length))
  }

  /**
    * Marks a batch of documents. Runs in parallel with other batches.
    * @param fname the processed file name
    * @param batch the documents to be marked
    * @return (the marked text of the documents, number of abstracts)
    */
  private def processBatch(fname: String,
                           batch: Seq[Doc]): (String, Int) = {
    val writer = new StringWriter(batch.map(_.chars).sum.toInt + 1024)
    val abstracts: Int = batch.map(doc => processLines(fname, doc.lines.iterator, doc.lang, writer)).sum

    (writer.toString, abstracts)
  }

  /**
    * Copies the lines into the destination, marking the abstract fields
    * @param fname the processed file name
    * @param docLines the lines of a document
    * @param lang the document language
    * @param dest the destination of the lines
    * @return the number of abstracts found
    */
  private def processLines(fname: String,
                           docLines: Iterator[String],
                           lang: Option[String],
                           dest: Writer): Int = {
    var abstracts: Int = 0

    while (docLines.hasNext) {
      val line = docLines.next()  // not trimmed, so the original indentation is kept
      if (startsWithIgnoringIndent(line, "<field name=\"ab")) {
        val lang1: Option[String] = abLangRegex.findFirstMatchIn(line).map(mat => mat.group(1).toLowerCase).
          orElse(lang).orElse(Some("en"))
        abstracts += 1
        processAbField(fname, lang1, line, docLines, dest)
      } else {
        dest.write(line)
        dest.write(lineSeparator)
      }
    }
    abstracts
  }

  /**
    * @param lines the lines of the input xml file
    * @return an iterator (no buffering) over the next lines up to and including the one having </doc>
    */
  private def untilDocEnd(lines: Iterator[String]): Iterator[String] = new Iterator[String] {
    private var finished: Boolean = false

    override def hasNext: Boolean = !finished && lines.hasNext

    override def next(): String = {
      val line: String = lines.next()
      finished = line.contains("</doc>")
      line
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
                             dest: Writer): Unit = {
    require (fname != null)
    require (openLine != null)
    require (lines != null)
    require (dest != null)

    val field: String = getAbField(openLine, lines)

    regex.findFirstMatchIn(field) match {
      case Some(mat) =>
        val prefix: String = mat.group(1)
        val noFmtContent: String = removeFmtHtmlMarks(mat.group(3)) // remove html formatting tags
        val marked: String = markSentence(noFmtContent, prefixes, lang)
        val rest: String = field.substring(mat.end)  // ex: "</doc>" in the same line of the field

        // the mark field is written right after the abstract field, before any following content
        dest.write(field.substring(0, mat.end))
        dest.write(lineSeparator)
        dest.write(prefix + "<field name=\"mark_" + mat.group(2) + "\">" + marked + "</field>")
        dest.write(lineSeparator)
        if (rest.trim.nonEmpty) {
          dest.write(rest)
          dest.write(lineSeparator)
        }
      case None =>
        println(s"WARNING: [$fname] mark field not created - unexpected abstract format: ${field.take(100)}")
        dest.write(field)
        dest.write(lineSeparator)
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
    // Only a segment followed by ':' can end with a prefix. Segments that do not end with
    // a prefix keep their original separator (ex: "3:1", "http://")
    def markSegment(split: String,
                    separator: String): String = {
      val (str: String, seqPos: Seq[Int]) = org.bireme.dh.Tools.uniformString2(split)
      prefixes.filter(str.endsWith).maxByOption(_.length) match {   // longest prefix wins
        case Some(prefix) =>
          val begPos: Int = seqPos(seqPos.length - prefix.length)
          val decsMarked: String = markDeCSDescriptors(split.substring(0, begPos), lang)
          decsMarked + "&lt;h2&gt;" + split.substring(begPos) + ":&lt;/h2&gt;"
        case None => markDeCSDescriptors(split, lang) + separator
      }
    }

    if (prefixes.isEmpty) markDeCSDescriptors(sentence, lang)
    else {
      val builder = new StringBuilder()
      var last: Int = 0

      colonRegex.findAllMatchIn(sentence).foreach {
        mat =>
          builder.append(markSegment(sentence.substring(last, mat.start), mat.matched))
          last = mat.end
      }
      builder.append(markDeCSDescriptors(sentence.substring(last), lang))
      builder.toString
    }
  }

  /**
    * Given an input text, mark all DeCS descriptors and synonyms
    * @param text input text
    * @param lang abstract text language
    * @return the input text marked
    */
  private def markDeCSDescriptors(text: String,
                                  lang: Option[String]): String = {
    highlighter match {
      case Some(hl) =>
        Try {
          val conf: Config = decsConfigs(lang.filter(scanLangs.contains))
          val (_, seq, _) = hl.highlight("", "", text, conf)
          val builder = new java.lang.StringBuilder(text.length + 48 * seq.size)
          val tend: Int = seq.foldLeft(0) {
            case (lpos, (termBegin: Int, termEnd: Int, id: String, _, _, _)) =>
              builder.append(text, lpos, termBegin)
                     .append("&lt;a class=\"decs\" id=\"").append(id).append("\"&gt;")
                     .append(text, termBegin, termEnd + 1)
                     .append("&lt;/a&gt;")
              termEnd + 1
          }
          if (tend < text.length) builder.append(text, tend, text.length)
          builder.toString
        } match {
          case Success(v) => v
          case Failure(exception) =>
            Console.err.println(s"DeCS marking failed, keeping the text unmarked - ${exception.toString}")
            text
        }
      case None => text
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

    val builder = new StringBuilder(openLine)
    var line: String = openLine

    while (!line.contains("</field>")) {
      if (!lines.hasNext) throw new IOException("</field> expected")
      if (builder.length > maxFieldChars)
        throw new IOException(s"</field> expected - abstract field bigger than $maxFieldChars chars")
      line = lines.next()
      builder.append('\n').append(line)  // keeps the line break so words are not glued together
    }
    builder.toString
  }

  /**
    * Converts all input characters into a-z, 0-9, '-' and spaces
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
      if (htmlFmtSet.contains(mat.group(1).toLowerCase)) Some("")
      else None

    require (str != null)
    fmtTagRegex.replaceSomeIn(str, replace)
  }

  /**
    * Same as line.trim.startsWith(prefix), but without creating a trimmed copy of the line
    */
  private def startsWithIgnoringIndent(line: String,
                                       prefix: String): Boolean = {
    var start: Int = 0
    while (start < line.length && line.charAt(start) <= ' ') start += 1
    line.startsWith(prefix, start)
  }
}
