/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import jakarta.servlet.{ServletConfig, ServletContext, ServletException}
import jakarta.servlet.http.{HttpServletRequest, HttpServletResponse}

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.file.{Files, Path}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import TestFiles.*

class MarkAbstractServletTest extends AnyFlatSpec with Matchers {

  /** Creates an implementation of the interface where each method call is answered by 'answer' */
  private def fake[T](cls: Class[T])(answer: PartialFunction[(String, Seq[AnyRef]), AnyRef]): T = {
    val handler = new InvocationHandler {
      override def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
        val call = (method.getName, Option(args).map(_.toSeq).getOrElse(Seq()))
        answer.applyOrElse(call, {
          case ("toString", _) => cls.getSimpleName
          case ("hashCode", _) => Integer.valueOf(System.identityHashCode(proxy))
          case ("equals", Seq(other)) => java.lang.Boolean.valueOf(proxy eq other)
          case _ => null
        })
      }
    }
    Proxy.newProxyInstance(cls.getClassLoader, Array(cls), handler).asInstanceOf[T]
  }

  private def config(params: Map[String, String]): ServletConfig = {
    val context = fake(classOf[ServletContext]) {
      case ("getInitParameter", Seq(name: String)) => params.get(name).orNull
    }
    fake(classOf[ServletConfig]) {
      case ("getServletContext", _) => context
      case ("getServletName", _) => "MarkAbstractServlet"
    }
  }

  /** The response status (200 if sendError was not called), message and body */
  private class Response {
    var status: Int = 200
    var message: String = ""
    val body = new StringWriter()

    val servletResponse: HttpServletResponse = fake(classOf[HttpServletResponse]) {
      case ("sendError", Seq(code: Integer, msg: String)) =>
        status = code
        message = msg
        null
      case ("getWriter", _) => new PrintWriter(body, true)
    }
  }

  private def request(params: Map[String, String]): HttpServletRequest =
    fake(classOf[HttpServletRequest]) {
      case ("getParameter", Seq(name: String)) => params.get(name).orNull
    }

  /** Runs the test with an initialized servlet whose BASE_DIR is 'base' (with directories base/in and base/out) */
  private def withServlet(test: (MarkAbstractServlet, Path) => Any): Unit = {
    val root: Path = tempDir("servlet-test")
    try {
      val base: Path = Files.createDirectories(root.resolve("base"))
      Files.createDirectories(base.resolve("in"))
      val servlet = new MarkAbstractServlet()
      servlet.init(config(Map("PREFIX_FILE_PATH" -> writePrefixFile(root).toString, "BASE_DIR" -> base.toString)))
      test(servlet, base)
    } finally deleteRecursively(root)
  }

  private def get(servlet: MarkAbstractServlet,
                  params: Map[String, String]): Response = {
    val response = new Response()
    servlet.doGet(request(params), response.servletResponse)
    response
  }

  "MarkAbstractServlet" should "not start without BASE_DIR" in {
    val root = tempDir("servlet-test")
    try {
      an [ServletException] should be thrownBy
        new MarkAbstractServlet().init(config(Map("PREFIX_FILE_PATH" -> writePrefixFile(root).toString)))
    } finally deleteRecursively(root)
  }

  it should "mark the files of a valid request" in {
    withServlet { (servlet, base) =>
      write(base.resolve("in"), "a.xml", xml(doc("ab" -> "Results: x")))
      val response = get(servlet, Map("inDir" -> base.resolve("in").toString, "xmlRegExp" -> ".*\\.xml",
                                      "outDir" -> base.resolve("out").toString))
      response.status shouldBe 200
      response.body.toString shouldBe "OK"
      marks(read(base.resolve("out/a.xml"))) shouldBe Seq(s"${h2("Results")} x")
    }
  }

  it should "answer 400 if a parameter is missing" in {
    withServlet { (servlet, base) =>
      val response = get(servlet, Map("xmlRegExp" -> ".*", "outDir" -> base.resolve("out").toString))
      response.status shouldBe 400
      response.message should include ("inDir")
    }
  }

  it should "answer 400 if a directory is outside BASE_DIR" in {
    withServlet { (servlet, base) =>
      val outside = Files.createDirectories(base.getParent.resolve("outside"))
      get(servlet, Map("inDir" -> outside.toString, "xmlRegExp" -> ".*",
                       "outDir" -> base.resolve("out").toString)).status shouldBe 400
      get(servlet, Map("inDir" -> base.resolve("in").toString, "xmlRegExp" -> ".*",
                       "outDir" -> outside.toString)).status shouldBe 400
    }
  }

  it should "answer 400 if a directory uses '..' to leave BASE_DIR" in {
    withServlet { (servlet, base) =>
      get(servlet, Map("inDir" -> base.resolve("in/../../").toString, "xmlRegExp" -> ".*",
                       "outDir" -> base.resolve("out").toString)).status shouldBe 400
    }
  }

  it should "answer 400 if a directory is a symbolic link to outside BASE_DIR" in {
    withServlet { (servlet, base) =>
      val outside = Files.createDirectories(base.getParent.resolve("outside"))
      val link = Files.createSymbolicLink(base.resolve("link"), outside)
      get(servlet, Map("inDir" -> link.toString, "xmlRegExp" -> ".*",
                       "outDir" -> base.resolve("out").toString)).status shouldBe 400
    }
  }

  it should "answer 400 if the days parameter is not a number" in {
    withServlet { (servlet, base) =>
      val response = get(servlet, Map("inDir" -> base.resolve("in").toString, "xmlRegExp" -> ".*",
                                      "outDir" -> base.resolve("out").toString, "days" -> "abc"))
      response.status shouldBe 400
      response.message should include ("days")
    }
  }

  it should "answer 500 if the processing fails" in {
    withServlet { (servlet, base) =>
      write(base.resolve("in"), "a.xml", header + "<add>\n  <doc>\n    <field name=\"ab\">Objective: x\n")
      val response = get(servlet, Map("inDir" -> base.resolve("in").toString, "xmlRegExp" -> ".*\\.xml",
                                      "outDir" -> base.resolve("out").toString))
      response.status shouldBe 500
      response.message should include ("</field> expected")
    }
  }
}
