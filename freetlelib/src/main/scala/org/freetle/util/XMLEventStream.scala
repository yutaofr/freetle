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
package org.freetle.util
import scala.io.Source
import scala.collection.immutable.LazyList
import org.codehaus.stax2.XMLStreamReader2
import com.ctc.wstx.stax.WstxInputFactory
import javax.xml.stream.{XMLStreamReader, XMLStreamConstants}
import java.lang.String
import java.io.{InputStream, Reader}
import java.net.URL
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import javax.xml.XMLConstants

//import javax.xml.stream.XMLInputFactory
//import javax.xml.stream.XMLStreamReader

/**
 * Helper class to read a Source as a Reader.
 */
class SourceReader(src: Source) extends Reader {
  override def read(arr : Array[Char], start : Int, sz : Int) : Int = {
    var i = start
    while (src.hasNext && i - start < sz) {
      arr(i) = src.next()
      i += 1
    }
    if (i - start == 0 && !src.hasNext)
      -1
    else
      i - start
  }
  
  override def close(): Unit = src.close()
  
}

/**
 * Helper class to read any char iterator as a Reader.
 */
class IteratorReader(iterator: =>Iterator[Char]) extends Reader {
  private var src = iterator
  override def read(arr : Array[Char], start : Int, sz : Int) : Int = {
    var i = start
    while (src.hasNext && i - start < sz) {
      arr(i) = src.next()
      i += 1
    }
    if (i - start == 0 && !src.hasNext)
      -1
    else
      i - start
  }

  override def close(): Unit = {
    src = Iterator.empty
  }

}

/**
 * Helper class retained for source compatibility with legacy Stream-based callers.
 */
class StreamReader(stream: =>Stream[Char]) extends IteratorReader(stream.iterator)

/**
 * Create a Source from a Char Iterator.
 */
object StreamSource {
  def fromIterator(s: Iterator[Char]): Source = {
    lazy val it = s
    new Source {
      override def reset() = fromIterator(s)
      val iter = it
    }
  }
}

object XMLEventStream {
  val factory =  new WstxInputFactory()
  factory.configureForSpeed()
  factory.getConfig.doCoalesceText(true)

  private def safeClose(closeable: AutoCloseable): Unit = {
    try {
      closeable.close()
    } catch {
      case _: Throwable => ()
    }
  }

}
/**
 * Transform a Source a XMLEvent Iterator for the purpose of making it a Stream afterward.
 * NB: You can create a stream from this using Stream.fromIterator().
 */
final class XMLEventStream(src: Any, xsdURL : String = null) extends Iterator[XMLEvent] with AutoCloseable {

  private val parserAndOwnedCloseables: (XMLStreamReader2, List[AutoCloseable]) = src match {
    case in : InputStream =>
      (XMLEventStream.factory.createXMLStreamReader(in).asInstanceOf[XMLStreamReader2], List(in))
    case stream : LazyList[_] =>
      val reader = new IteratorReader(stream.asInstanceOf[LazyList[Char]].iterator)
      (XMLEventStream.factory.createXMLStreamReader(reader).asInstanceOf[XMLStreamReader2], List(reader))
    case source : Source =>
      val sourceReader = new SourceReader(source)
      (XMLEventStream.factory.createXMLStreamReader("default.xml", sourceReader).asInstanceOf[XMLStreamReader2], List(sourceReader))
    case iterator : Iterator[_] =>
      val reader = new IteratorReader(iterator.asInstanceOf[Iterator[Char]])
      (XMLEventStream.factory.createXMLStreamReader(reader).asInstanceOf[XMLStreamReader2], List(reader))
    case unsupported =>
      throw new IllegalArgumentException("Unsupported input type for XMLEventStream: " + unsupported.getClass.getName)
  }

  val input : XMLStreamReader2 = parserAndOwnedCloseables._1
  private val ownedCloseables: List[AutoCloseable] = parserAndOwnedCloseables._2

  if (xsdURL != null) {
    val validationFactory = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
    val xmlSchema: XMLValidationSchema = validationFactory.createSchema(new URL(xsdURL))
    input.validateAgainst(xmlSchema)
  }

  type Attributes = Map[QName, String]
  type Namespaces = Map[String,  String]

  @inline private def fromJavaQName(qn : javax.xml.namespace.QName) : org.freetle.util.QName = {
    new QName(namespaceURI = qn.getNamespaceURI,
      localPart = qn.getLocalPart,
      prefix = qn.getPrefix)
  }
  
  @inline private def buildAttributes(input : XMLStreamReader) : Attributes = {
    (0 until input.getAttributeCount).iterator
      .map(x => (fromJavaQName(input.getAttributeName(x)), input.getAttributeValue(x)))
      .toMap
  }
  
  @inline private def buildNamespaces(input : XMLStreamReader) : Namespaces = {
    (0 until input.getNamespaceCount).iterator
      .map(x => (Option(input.getNamespacePrefix(x)).getOrElse(XMLConstants.DEFAULT_NS_PREFIX), input.getNamespaceURI(x)))
      .toMap
  }

  @inline private def buildEvent(input:XMLStreamReader) : Option[XMLEvent] = {
    val eventType = input.getEventType
		  val event : Option[XMLEvent] = eventType match {
		    case XMLStreamConstants.START_ELEMENT => Some(new EvElemStart(fromJavaQName(input.getName),
		                                                                 buildAttributes(input),
		                                                                 buildNamespaces(input)
		                                                                 ))
	      case XMLStreamConstants.END_ELEMENT => Some(new EvElemEnd(fromJavaQName(input.getName)))
	      case XMLStreamConstants.CHARACTERS => Some(new EvText(input.getText))
	      case XMLStreamConstants.COMMENT => Some(new EvComment(input.getText))
	      case XMLStreamConstants.PROCESSING_INSTRUCTION => Some(new EvProcInstr(input.getPITarget, input.getPIData))
	      case XMLStreamConstants.START_DOCUMENT => None
	        // TODO Add start and end document.
		    case _ => None
		  }
    event.foreach { e =>
      e.location = input.getLocation
      e.namespaceContext = input.getNamespaceContext
    }
    event
  }

  private var prefetchedEvent : Option[XMLEvent] = None
  private var exhausted : Boolean = false
  private var closed : Boolean = false

  override def close(): Unit = synchronized {
    if (!closed) {
      closed = true
      exhausted = true
      prefetchedEvent = None
      try {
        input.close()
      } catch {
        case _: Throwable => ()
      }
      ownedCloseables.foreach(XMLEventStream.safeClose)
    }
  }

  private def fetchNextEvent(): Option[XMLEvent] = {
    var event = buildEvent(input)
    while (event.isEmpty && input.hasNext) {
      input.next()
      event = buildEvent(input)
    }
    if (event.isEmpty) {
      exhausted = true
      close()
    }
    event
  }

  def next(): XMLEvent = {
    if (!hasNext) throw new NoSuchElementException("No more XML events")
    val event = prefetchedEvent.get
    prefetchedEvent = None
    if (input.hasNext) {
      input.next()
    } else {
      exhausted = true
      close()
    }
    event
  }

  override def hasNext: Boolean = {
    if (closed) {
      false
    } else if (prefetchedEvent.isDefined) {
      true
    } else if (exhausted) {
      false
    } else {
      prefetchedEvent = fetchNextEvent()
      prefetchedEvent.isDefined
    }
  }
}
