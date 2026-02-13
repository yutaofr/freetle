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

import javax.xml.namespace.NamespaceContext
import javax.xml.XMLConstants
import java.io._
import javax.xml.stream.{XMLStreamWriter, Location}

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
@SerialVersionUID(32001)
sealed abstract class XMLEvent extends Externalizable {
  var location : Location = null
  var namespaceContext : NamespaceContext = null
  override def toString: String = toStream.mkString

  final def toStream:  Stream[Char] = {
    var sb = new StringWriter()
    appendWriter(sb)
    sb.toString.toStream
  }

  /**
   * Testing purposes only. Use appendTo instead.
   * @param writer
   */
  def appendWriter(writer : Writer)

  def appendTo(serializer : XMLStreamWriter)
}

private object XMLEventCompat {
  @inline def orEmpty(value: String): String = if (value == null) "" else value
}

/**
 * This class represents a qualified name.
 */
@SerialVersionUID(32002)
case class QName(var namespaceURI : String = XMLConstants.NULL_NS_URI,
                 var localPart: String = null,
                 var prefix : String = XMLConstants.DEFAULT_NS_PREFIX) extends Externalizable {
  def readExternal(in: ObjectInput) {
    namespaceURI = in.readUTF
    localPart = in.readUTF
    prefix = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(XMLEventCompat.orEmpty(this.namespaceURI))
    out.writeUTF(XMLEventCompat.orEmpty(this.localPart))
    out.writeUTF(XMLEventCompat.orEmpty(this.prefix))
  }
}

/** An element representing an openning tag */
@SerialVersionUID(32003)
case class EvElemStart(var name : QName = null, var attributes : Map[QName, String] = null, var namespaces : Map[String, String] = null) extends XMLEvent {
  final def this() = this(null, null)
  private def safeName: QName = Option(name).getOrElse(QName(localPart = ""))
  private def safeAttributes: Map[QName, String] = Option(attributes).getOrElse(Map.empty)
  private def safeNamespaces: Map[String, String] = Option(namespaces).getOrElse(Map.empty)

  private final def buildAttrStringBuffer(sb :Writer)(j : (QName, String)) {
    val attrName = Option(j._1).getOrElse(QName(localPart = ""))
    sb.append(' ')
    val prefix = XMLEventCompat.orEmpty(attrName.prefix)
    if (prefix.length != 0) {
      sb.append(prefix)
      sb.append(':')
    }
    sb.append(XMLEventCompat.orEmpty(attrName.localPart))
    sb.append('=')
    sb.append('"')
    sb.append(XMLEventCompat.orEmpty(j._2))
    sb.append('"')
  }

  private final def buildNamespaceStringBuffer(sb :Writer)(j : (String, String)) {
    val prefix = XMLEventCompat.orEmpty(j._1)
    sb.append(' ')
    sb.append("xmlns")
    if (!XMLConstants.DEFAULT_NS_PREFIX.equals(prefix)) {
      sb.append(':')
      sb.append(prefix)
    }
    sb.append('=')
    sb.append('"')
    sb.append(XMLEventCompat.orEmpty(j._2))
    sb.append('"')
  }

  final def appendWriter(sb: Writer) {
    val currentName = safeName
    val namePrefix = XMLEventCompat.orEmpty(currentName.prefix)
    sb.append('<')
    if (namePrefix.length != 0) {
      sb.append(namePrefix)
      sb.append(':')
    }
    sb.append(XMLEventCompat.orEmpty(currentName.localPart))

    // Attributes
    safeAttributes.foreach(
      buildAttrStringBuffer(sb)
    )

    safeNamespaces.foreach(
     buildNamespaceStringBuffer(sb)
    )
    sb.append('>')
  }

  final def appendTo(serializer : XMLStreamWriter) {
    val currentName = safeName
    serializer.writeStartElement(
      XMLEventCompat.orEmpty(currentName.prefix),
      XMLEventCompat.orEmpty(currentName.localPart),
      XMLEventCompat.orEmpty(currentName.namespaceURI)
    )
    safeAttributes.foreach[Unit](
      attr => {
        val attributeName = Option(attr._1).getOrElse(QName(localPart = ""))
        serializer.writeAttribute(
          XMLEventCompat.orEmpty(attributeName.prefix),
          XMLEventCompat.orEmpty(attributeName.namespaceURI),
          XMLEventCompat.orEmpty(attributeName.localPart),
          XMLEventCompat.orEmpty(attr._2)
        )
      })
    safeNamespaces.foreach[Unit](
      namespace => {
        val prefix = XMLEventCompat.orEmpty(namespace._1)
        val uri = XMLEventCompat.orEmpty(namespace._2)
        if (XMLConstants.DEFAULT_NS_PREFIX.equals(prefix))
          serializer.writeDefaultNamespace(uri)
        else
          serializer.writeNamespace(prefix, uri)
      })
  }

  def readExternal(in: ObjectInput) {
    this.name = new QName()
    this.name.readExternal(in)
    val sizeAttr = in.readInt
    this.attributes = (1 to sizeAttr).map(x => {
      val name = new QName()
      name.readExternal(in)
      val value = in.readUTF
      (name, value)
    }).toMap[QName, String]
    val sizeNamespc = in.readInt
    this.namespaces = (1 to sizeNamespc).map( x => {
      val prefix = in.readUTF
      val namespaceURI = in.readUTF
      (prefix, namespaceURI)
    }).toMap[String, String]
  }

  def writeExternal(out: ObjectOutput) {
    safeName.writeExternal(out)
    val attrs = safeAttributes
    out.writeInt(attrs.size)
    attrs.foreach[Unit]( x => {
      val (attributeName : QName, attributeValue : String) = x
      Option(attributeName).getOrElse(QName(localPart = "")).writeExternal(out)
      out.writeUTF(XMLEventCompat.orEmpty(attributeValue))
    })
    val namespc : Map[String, String]= safeNamespaces
    out.writeInt(namespc.size)
    namespc.foreach[Unit]( x => {
      val (prefix : String, namespaceURI : String) = x
      out.writeUTF(XMLEventCompat.orEmpty(prefix))
      out.writeUTF(XMLEventCompat.orEmpty(namespaceURI))
    })
    
  }



}

/** An element representing a closing tag */
@SerialVersionUID(32004)
case class EvElemEnd(var name : QName) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    val currentName = Option(name).getOrElse(QName(localPart = ""))
    val prefix = XMLEventCompat.orEmpty(currentName.prefix)
    sb.append('<')
    sb.append('/')
    if (prefix.length != 0) {
      sb.append(prefix)
      sb.append(':')
    }
    sb.append(XMLEventCompat.orEmpty(currentName.localPart))
    sb.append('>')
  }

  final def appendTo(serializer : XMLStreamWriter) {
    serializer.writeEndElement()
  }

  def readExternal(in: ObjectInput) {
    this.name = new QName()
    this.name.readExternal(in)
  }

  def writeExternal(out: ObjectOutput) {
    Option(this.name).getOrElse(QName(localPart = "")).writeExternal(out)
  }
}
/** A text node is encountered */
@SerialVersionUID(32005)
case class EvText(var text : String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append(XMLEventCompat.orEmpty(text))
  }

  final def appendTo(serializer : XMLStreamWriter) {
    serializer.writeCharacters(XMLEventCompat.orEmpty(text))
  }

  def readExternal(in: ObjectInput) {
    this.text = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(XMLEventCompat.orEmpty(this.text))
  }
}

/** An entity reference is encountered */
@SerialVersionUID(32006)
case class EvEntityRef(var entity: String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append('&')
    sb.append(XMLEventCompat.orEmpty(entity))
    sb.append(';')
  }

  final def appendTo(serializer : XMLStreamWriter) {
    serializer.writeEntityRef(XMLEventCompat.orEmpty(entity))
  }

  def readExternal(in: ObjectInput) {
    this.entity = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(XMLEventCompat.orEmpty(this.entity))
  }
}

/** A processing instruction is encountered */
@SerialVersionUID(32007)
case class EvProcInstr(var target: String, var text: String) extends XMLEvent {
  final def this() = this(null, null)
  final def appendWriter(sb: Writer) {}

  final def appendTo(serializer : XMLStreamWriter) {
    serializer.writeProcessingInstruction(XMLEventCompat.orEmpty(target), XMLEventCompat.orEmpty(text))
  }

  final def readExternal(in: ObjectInput) {
    this.target = in.readUTF
    this.text = in.readUTF
  }

  final def writeExternal(out: ObjectOutput) {
    out.writeUTF(XMLEventCompat.orEmpty(this.target))
    out.writeUTF(XMLEventCompat.orEmpty(this.text))
  }
}

/** A comment is encountered */
@SerialVersionUID(32008)
case class EvComment(var text: String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append("<!-- ")
    sb.append(XMLEventCompat.orEmpty(text))
    sb.append(" -->")
  }

  final def appendTo(serializer : XMLStreamWriter) {
    serializer.writeComment(XMLEventCompat.orEmpty(text))
  }

  final def readExternal(in: ObjectInput) {
    this.text = in.readUTF
  }

  final def writeExternal(out: ObjectOutput) {
    out.writeUTF(XMLEventCompat.orEmpty(this.text))
  }
}
