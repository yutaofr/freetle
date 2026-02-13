/*
 * Copyright 2010-2013 Lucas Bruand
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.freetle

import java.io.ByteArrayInputStream
import org.freetle.util.{EvText, XMLEvent}
import org.junit.Assert._
import org.junit.Test

case class XMLMigrationCompatContext(value: String = "default")

@Test
class CPSXMLMigrationCompatibilityTest extends CPSXMLModel[XMLMigrationCompatContext] {
  private def labels(stream: CPSStream): List[String] = {
    stream.toList.map {
      case (Some(event), isResult) => s"$isResult:${event.getClass.getSimpleName}:${event.toString}"
      case (None, isResult) => s"$isResult:None"
    }
  }

  @Test
  def testInputStreamLegacyAndOptionOverloadsAreEquivalent(): Unit = {
    val xml = "<root><v>1</v></root>"
    val fromLegacy = XMLResultStreamUtils.loadXMLResultStream(new ByteArrayInputStream(xml.getBytes("UTF-8")), null.asInstanceOf[String])
    val fromOption = XMLResultStreamUtils.loadXMLResultStream(new ByteArrayInputStream(xml.getBytes("UTF-8")), Option.empty[String])
    assertEquals(labels(fromLegacy), labels(fromOption))
  }

  @Test
  def testCharLegacyAndLazyListOverloadsAreEquivalent(): Unit = {
    val xml = "<root><v>1</v></root>"
    val fromLegacy = XMLResultStreamUtils.loadXMLResultStream(xml.toStream)
    val fromLazyList = XMLResultStreamUtils.loadXMLResultStream(xml.to(LazyList))
    assertEquals(labels(fromLegacy), labels(fromLazyList))
  }

  @Test
  def testLegacyEventStreamPushOverloadStillWorks(): Unit = {
    val legacyEvents: Stream[XMLEvent] = Stream(new EvText("legacy"))
    val transform = >(legacyEvents)
    val out = transform(new CFilterIdentity(), new CFilterIdentity())(LazyList.empty, XMLMigrationCompatContext())
    assertTrue(out.nonEmpty)
    assertTrue(out.head._2)
    out.head._1 match {
      case Some(text: EvText) => assertEquals("legacy", text.text)
      case x => fail("Expected EvText, got: " + x)
    }
  }
}

case class TranslateMigrationCompatContext(value: String = "default")

@Test
class CPSTranslateMigrationCompatibilityTest extends CPSTranslateModel[TranslateMigrationCompatContext] {
  private def labels(stream: CPSStream): List[String] = {
    stream.toList.map {
      case (Some(Left(c)), isResult) => s"$isResult:Left:$c"
      case (Some(Right(event)), isResult) => s"$isResult:Right:${event.getClass.getSimpleName}:${event.toString}"
      case (None, isResult) => s"$isResult:None"
    }
  }

  @Test
  def testLegacyContextStreamOverloadStillWorks(): Unit = {
    val transform = >((context: TranslateMigrationCompatContext) => Stream(new EvText(context.value)))
    val out = transform(new CFilterIdentity(), new CFilterIdentity())(LazyList.empty, TranslateMigrationCompatContext("legacy"))
    assertTrue(out.nonEmpty)
    assertTrue(out.head._2)
    out.head._1 match {
      case Some(Right(text: EvText)) => assertEquals("legacy", text.text)
      case x => fail("Expected Right(EvText), got: " + x)
    }
  }

  @Test
  def testLegacyAndLazyListCharLoadersAreEquivalent(): Unit = {
    val input = "abc123"
    val fromLegacy = ResultStreamUtils.loadXMLResultStream(input.toStream)
    val fromLazyList = ResultStreamUtils.loadXMLResultStream(input.to(LazyList))
    assertEquals(labels(fromLegacy), labels(fromLazyList))
  }
}
