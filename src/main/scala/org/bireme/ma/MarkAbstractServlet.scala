/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import jakarta.servlet.ServletConfig
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}


class MarkAbstractServlet extends HttpServlet {
  var mabs: MarkAbstract = _

  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val prefixFile: String = config.getServletContext.getInitParameter("PREFIX_FILE_PATH")
    if ((prefixFile == null) || prefixFile.isEmpty)
      throw new NullPointerException(s"PREFIX_FILE_PATH = $prefixFile")

    val deCSPath: Option[String] = Option(config.getServletContext.getInitParameter("DECS_PATH"))

    mabs = new MarkAbstract(prefixFile, deCSPath)

    println("MarkAbstract servlet is now listening ...")
  }

  /**
    * Process get http requisition
    * @param request http request object
    * @param response http response object
    */
  override def doGet(request: HttpServletRequest,
                     response: HttpServletResponse): Unit = processRequest(request, response)

  /**
    * Process post http requisition
    * @param request http request object
    * @param response http response object
    */
  override def doPost(request: HttpServletRequest,
                      response: HttpServletResponse): Unit = processRequest(request, response)

  /**
    *  Process get or post requisition
    * @param request http request object
    * @param response http response object
    */
  private def processRequest(request: HttpServletRequest,
                             response: HttpServletResponse): Unit = {
    Option(request.getParameter("inDir")) match {
      case Some(inDir) =>
        Option(request.getParameter("xmlRegExp")) match {
          case Some(xmlRegExp) =>
            Option(request.getParameter("outDir")) match {
              case Some(outDir) =>
                val days: Option[Int] = Option(request.getParameter("days")).map(_.toInt)
                mabs.processFiles(inDir, xmlRegExp, outDir, days)
              case None => response.sendError(400, "Missing output directory (outDir parameter)")
            }
          case None => response.sendError(400, "Missing regular expression file filter (xmlRegExp parameter)")
        }
      case None => response.sendError(400, "Missing input directory (inDir parameter)")
    }
  }
}
