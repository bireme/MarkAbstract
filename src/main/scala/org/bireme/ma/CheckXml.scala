/*=========================================================================

    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.ma

import javax.xml.parsers.SAXParserFactory
import org.xml.sax.{ErrorHandler,InputSource,SAXParseException}

import scala.util.{Failure,Success,Try}

class SimpleErrorHandler extends ErrorHandler {
  var errMsg = ""

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
  def check(xml: String): Option[String] = {
    require (xml != null)

    Try {
      val factory = SAXParserFactory.newInstance()
      factory.setValidating(false)
      factory.setNamespaceAware(true)

      val parser = factory.newSAXParser()
      val reader = parser.getXMLReader
      val handler = new SimpleErrorHandler()

      reader.setErrorHandler(handler)
      reader.parse(new InputSource(xml))

      handler.getErrMsg
    } match {
      case Success(errMess) => errMess
      case Failure(ex) => Some(ex.getMessage)
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
    case Some(msg) => println(s"[${args(0)}] ERROR: $msg")
    case None => println(s"[${args(0)}] - OK")
  }
}
