/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import jakarta.servlet.{ServletConfig, ServletException}
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success}


class MarkAbstractServlet extends HttpServlet {
  private var mabs: MarkAbstract = _
  private var baseDir: Path = _   // inDir and outDir must be inside this directory

  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val prefixFile: String = config.getServletContext.getInitParameter("PREFIX_FILE_PATH")
    if ((prefixFile == null) || prefixFile.isEmpty)
      throw new NullPointerException(s"PREFIX_FILE_PATH = $prefixFile")

    val base: String = config.getServletContext.getInitParameter("BASE_DIR")
    if ((base == null) || base.isEmpty)
      throw new ServletException("Missing BASE_DIR parameter (directory allowed for inDir and outDir)")
    baseDir = realPath(base)

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
    * @param dir a directory path
    * @return the absolute path with symbolic links resolved (if it exists) and without '..'
    */
  private def realPath(dir: String): Path = {
    val path: Path = Paths.get(dir).toAbsolutePath.normalize
    if (Files.exists(path)) path.toRealPath() else path
  }

  /**
    *  Process get or post requisition
    * @param request http request object
    * @param response http response object
    */
  private def processRequest(request: HttpServletRequest,
                             response: HttpServletResponse): Unit = {
    def param(name: String,
              description: String): Either[String, String] =
      Option(request.getParameter(name)).toRight(s"Missing $description ($name parameter)")

    def insideBaseDir(name: String,
                      dir: String): Either[String, String] =
      Either.cond(realPath(dir).startsWith(baseDir), dir, s"$name must be inside $baseDir")

    val parameters: Either[String, (String, String, String, Option[Int])] = for {
      inDir <- param("inDir", "input directory").flatMap(insideBaseDir("inDir", _))
      xmlRegExp <- param("xmlRegExp", "regular expression file filter")
      outDir <- param("outDir", "output directory").flatMap(insideBaseDir("outDir", _))
      days <- Option(request.getParameter("days")) match {
        case Some(ds) => ds.toIntOption.map(Some(_)).toRight(s"Invalid days parameter: $ds")
        case None => Right(None)
      }
    } yield (inDir, xmlRegExp, outDir, days)

    parameters match {
      case Left(message) => response.sendError(HttpServletResponse.SC_BAD_REQUEST, message)
      case Right((inDir, xmlRegExp, outDir, days)) =>
        mabs.processFiles(inDir, xmlRegExp, outDir, days) match {
          case Success(_) =>
            response.setContentType("text/plain;charset=utf-8")
            response.getWriter.write("OK")
          case Failure(exception) =>
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, exception.toString)
        }
    }
  }
}
