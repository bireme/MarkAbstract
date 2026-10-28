package org.bireme.ma

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.jdk.CollectionConverters.*
import scala.util.Success
import TestFiles.*

/** Exercises the HighlightServlet protocol using a local HTTP server. */
class MarkAbstractServerTest extends AnyFlatSpec with Matchers {
  "MarkAbstract" should "post every segment and construct DeCS links from server positions" in {
    val root = tempDir("mark-abstract-server")
    val requests = new ConcurrentLinkedQueue[Map[String, String]]()
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/serv", (exchange: HttpExchange) => {
      val body = new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
      val params = body.split("&").map { item =>
        val pair = item.split("=", 2)
        URLDecoder.decode(pair(0), StandardCharsets.UTF_8) ->
          URLDecoder.decode(if (pair.length == 2) pair(1) else "", StandardCharsets.UTF_8)
      }.toMap
      requests.add(params)
      val document = params("document")
      val status = if (document.isEmpty) 400 else 200
      val json = if (document.contains("saúde")) {
        val begin = document.indexOf("saúde")
        """{"positions":[{"begin":""" + begin + ""","end":""" + (begin + 4) +
          ""","id":"D0001"}]}"""
      } else """{"positions":[]}"""
      val bytes = json.getBytes(StandardCharsets.UTF_8)
      exchange.sendResponseHeaders(status, bytes.length)
      val output = exchange.getResponseBody
      try output.write(bytes) finally output.close()
    })
    server.start()
    try {
      val prefixFile = write(root, "prefixes.txt", "objective\n")
      val in = Files.createDirectories(root.resolve("in"))
      val out = root.resolve("out")
      write(in, "a.xml", xml(doc("la" -> "pt", "ab" -> "Objective: saúde")))
      val url = "http://127.0.0.1:" + server.getAddress.getPort + "/serv"
      val marker = new MarkAbstract(prefixFile.toString, None, 1, Some(url))

      marker.processFiles(in.toString, ".*\\.xml", out.toString, None) shouldBe Success(())
      marks(read(out.resolve("a.xml"))) shouldBe Seq(h2("Objective") + " " + decs("D0001", "saúde"))
      val received = requests.iterator().asScala.toSeq
      received.map(_("document")) shouldBe Seq("", " saúde")
      received.foreach { params =>
        params("scanLang") shouldBe "pt"
        params("showPositions") shouldBe "true"
        params("showText") shouldBe "false"
        params("showDescriptors") shouldBe "false"
        params("showScores") shouldBe "false"
      }
    } finally {
      server.stop(0)
      deleteRecursively(root)
    }
  }

  it should "reject a local index and a server URL together before opening the index" in {
    val root = tempDir("mark-abstract-server-conflict")
    try {
      val prefixFile = writePrefixFile(root)
      an [IllegalArgumentException] should be thrownBy
        new MarkAbstract(prefixFile.toString, Some("/missing/index"), 1, Some("http://localhost/serv"))
    } finally deleteRecursively(root)
  }

  it should "show usage when both command-line options are specified" in {
    val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder(java, "-cp", System.getProperty("java.class.path"),
      "org.bireme.ma.MarkAbstract", "prefixes", "input", ".*", "output",
      "-deCSPath=/missing/index", "-deCSServer=http://localhost/serv")
      .redirectErrorStream(true).start()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    process.waitFor() shouldBe 1
    output should include ("usage: MarkAbstract")
    output should include ("-deCSServer")
  }
}
