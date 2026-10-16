/*=========================================================================
    MarkAbstract © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/MarkAbstract/blob/master/LICENSE.txt
  ==========================================================================*/

package org.bireme.ma

import java.nio.file.Path
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xml.sax.SAXParseException

import scala.util.Success

import TestFiles.*

class CheckXmlTest extends AnyFlatSpec with Matchers {
  private def withDir(test: Path => Any): Unit = {
    val dir: Path = tempDir("checkxml-test")
    try test(dir) finally deleteRecursively(dir)
  }

  "CheckXml" should "accept a well formed xml" in {
    withDir { dir =>
      val file = write(dir, "a.xml", xml(doc("ab" -> "Results: x")))
      new CheckXml().check(file.toString) shouldBe Success("")
    }
  }

  it should "fail a malformed xml, reporting the line of the error" in {
    withDir { dir =>
      val file = write(dir, "a.xml", "<add>\n<doc>\n<field name=\"ab\">x</field>\n</add>\n")
      val result = new CheckXml().check(file.toString)

      result.failed.get shouldBe a [SAXParseException]
      result.failed.get.getMessage should startWith ("line 4:")
    }
  }

  it should "not load external entities (XXE)" in {
    withDir { dir =>
      // if the external entity were loaded, the missing file would make the check fail
      val file = write(dir, "a.xml", "<?xml version=\"1.0\"?>\n" +
        "<!DOCTYPE a [<!ENTITY x SYSTEM \"file:///this/file/does/not/exist.txt\">]>\n<a>&x;</a>\n")
      new CheckXml().check(file.toString) shouldBe Success("")
    }
  }
}
