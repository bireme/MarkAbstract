/*=========================================================================

    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.ma

import javax.xml.parsers.{SAXParser, SAXParserFactory}
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException, XMLReader}

import scala.util.{Failure, Success, Try}

class SimpleErrorHandler extends ErrorHandler {
  private var errMsg = ""

  def warning(e: SAXParseException): Unit = {
    errMsg = e.getMessage
  }

  def error(e: SAXParseException): Unit = {
    errMsg = e.getMessage
  }

  def fatalError(e: SAXParseException): Unit = {
    errMsg = e.getMessage
  }

  def getErrMsg: Option[String] = if (errMsg.isEmpty) None else Some(errMsg)
}

class CheckXml {
  /**
    * Check if a xml file is well formed
    * @param xml input xml file
    * @return Success(error message) or Failure
    */
  def check(xml: String): Try[String] = {
    require (xml != null)

    Try {
      val factory = SAXParserFactory.newInstance()
      factory.setValidating(false)
      factory.setNamespaceAware(false)
      factory.setSchema(null)

      val parser: SAXParser = factory.newSAXParser()
      val reader: XMLReader = parser.getXMLReader
      val handler: SimpleErrorHandler = new SimpleErrorHandler()

      reader.setErrorHandler(handler)
      reader.parse(new InputSource(xml))

      handler.getErrMsg.getOrElse("")
    }
  }
}

object CheckXml extends App {
  private def usage(): Unit = {
    Console.err.println("usage: CheckXml <filename>")
    System.exit(1)
  }

  if (args.length != 1) usage()

  (new CheckXml).check(args(0)) match {
    case Failure(exception) => println(s"[${args(0)}] ERROR: ${exception.toString}")
    case Success(msg) => println(s"[${args(0)}][$msg] - OK")
  }
}
