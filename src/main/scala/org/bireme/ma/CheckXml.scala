/*=========================================================================

    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.ma

import java.io.File
import javax.xml.XMLConstants
import javax.xml.parsers.{SAXParser, SAXParserFactory}
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException, XMLReader}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/**
  * Collects the warnings and stops the parsing at the first error
  */
class SimpleErrorHandler extends ErrorHandler {
  private val warnings: ArrayBuffer[String] = ArrayBuffer()

  /**
    * Formats a parser diagnostic with its source line number.
    *
    * @param e parser diagnostic to format
    * @return diagnostic text prefixed by the line number
    */
  private def format(e: SAXParseException): String = s"line ${e.getLineNumber}: ${e.getMessage}"

  /**
    * Records a non-fatal parser warning.
    *
    * @param e parser warning to record
    */
  def warning(e: SAXParseException): Unit = {
    warnings += format(e)
  }

  /**
    * Stops parsing when the parser reports an XML error.
    *
    * @param e parser error to rethrow with formatted diagnostic text
    * @throws SAXParseException always, with the formatted error message
    */
  def error(e: SAXParseException): Unit = {
    throw new SAXParseException(format(e), null, e)
  }

  /**
    * Stops parsing when the parser reports a fatal XML error.
    *
    * @param e fatal parser error to rethrow with formatted diagnostic text
    * @throws SAXParseException always, with the formatted error message
    */
  def fatalError(e: SAXParseException): Unit = {
    throw new SAXParseException(format(e), null, e)
  }

  /**
    * Returns the warnings collected during parsing.
    *
    * @return immutable sequence of formatted warning messages
    */
  def getWarnings: Seq[String] = warnings.toSeq
}

class CheckXml {
  /**
    * Check if a xml file is well formed
    * @param xml input xml file
    * @return Success(warning messages) or Failure if the xml is not well formed
    */
  def check(xml: String): Try[String] = {
    require (xml != null)

    Try {
      val factory = SAXParserFactory.newInstance()
      factory.setValidating(false)
      factory.setNamespaceAware(false)
      factory.setSchema(null)
      factory.setXIncludeAware(false)

      // Avoid XXE: external entities and DTDs are never loaded
      factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
      factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
      factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)

      val parser: SAXParser = factory.newSAXParser()
      val reader: XMLReader = parser.getXMLReader
      val handler: SimpleErrorHandler = new SimpleErrorHandler()

      reader.setErrorHandler(handler)
      reader.parse(new InputSource(new File(xml).toURI.toString))

      handler.getWarnings.mkString("; ")
    }
  }
}

object CheckXml {
  /** Prints the command-line usage information and exits with a failure status. */
  private def usage(): Unit = {
    Console.err.println("usage: CheckXml <filename>")
    System.exit(1)
  }

  // 'extends App' does not work in Scala 3 ('args' is null), so main is explicit
  /**
    * Validates the XML file named by the command-line argument and prints the result.
    *
    * @param args exactly one XML file path
    */
  def main(args: Array[String]): Unit = {
    if (args.length != 1) usage()

    (new CheckXml).check(args(0)) match {
      case Failure(exception) => println(s"[${args(0)}] ERROR: ${exception.toString}")
      case Success(msg) => println(s"[${args(0)}][$msg] - OK")
    }
  }
}
