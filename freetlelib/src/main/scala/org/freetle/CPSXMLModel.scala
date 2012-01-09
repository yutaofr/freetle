 /*
  * Copyright 2010-2012 Lucas Bruand
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

import util._
import java.io._
import xml.{MetaData, Node, NodeSeq}

/**
 * This is a streaming Continuation Passing Transformation model.
 * It is capable of working over XML Events.
 */
class CPSXMLModel[@specialized Context] extends CPSModel[XMLEvent, Context] {

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilter does return the matching end bracket.
   */
  class DeepFilter extends StatefulSelector[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilter() })
    def conditionToStop(depth: Int) = (depth <= 0)

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _)) => +1
      case Some(EvElemEnd(_)) => -1
      case _ => 0
    })
    
    def initialState = 1
  }

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilteruntil does not return the matching end bracket.
   */
  class DeepFilterUntil extends StatefulSelectorUntil[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilter() })
    def conditionToStop(depth: Int) = (depth <= 0)

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _)) => +1
      case Some(EvElemEnd(_)) => -1
      case _ => 0
    })

    def initialState = 1
  }

  /**
   * A base class to load text tokens to context.
   */
  abstract class TakeTextToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
    
    @inline def apply(stream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(stream)
        (sr.head._1.get) match {
          case EvText(txt) =>
            (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(txt, context))
          case _ => (stream, context)
        }
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  /**
   * A base class to load attributes values to context.
   */
  abstract class TakeAttributesToContext(matcher : EvStartMatcher) extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(stream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(stream)
        val elem = sr.head._1.get
        if (matcher(elem)) {
          (elem) match {
            case EvElemStart(name, attributes)  =>
              (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(name, attributes, context))
            case _ => (stream, context)
          }
        } else {
          (stream, context)
        }
      }
    }
    def pushToContext(name : QName, attributes : Map[QName, String], context : Context) : Context
  }

  /**
   * A companion object to PushNode class.
   * Internal.
   */
  private object PushNode {

    def serializeXML(nodeSeq : NodeSeq) : Stream[XMLEvent] = {
      ((nodeSeq map( serializeNodeXML(_))).toStream.flatten)
    }

    private def createAttr(metaData : MetaData) : Map[QName, String] = {
      null
    }

    def serializeNodeXML(node : Node) : Stream[XMLEvent] =
      node match {
      case elem :  scala.xml.Elem //(prefix, label, attributes, scope, children)
            => {
                  val qName: QName = if (elem.prefix == null || elem.prefix.isEmpty)
                                        new QName(elem.scope.getURI(null), elem.label)
                                     else
                                        new QName(elem.scope.getURI(elem.prefix), elem.label, elem.prefix)
                  Stream.cons( new EvElemStart(qName,  createAttr(elem.attributes)),
                          Stream.concat(serializeXML(elem.child),
                            Stream(new EvElemEnd(qName))))
                }
      case text : scala.xml.Text => Stream(new EvText(text.text))
      case comment : scala.xml.Comment => Stream(new EvComment(comment.text))
      case pi : scala.xml.ProcInstr => Stream(new EvProcInstr(pi.target, pi.proctext))
      case entityRef : scala.xml.EntityRef => Stream(new EvEntityRef(entityRef.entityName))
      case atom : scala.xml.Atom[Any] => Stream(new EvText(atom.text))
      case _ => Stream(new EvText("error" + node.getClass)) // TODO Throw exception.
    }
  }
  /**
   * Push a scala xml content down the pipeline.
   */
  @SerialVersionUID(599494944949L + 10 *19L)
  class PushNode(nodeSeq: Option[Context] => NodeSeq) extends PushFromContext(
      ((x :Context) => Some(x)) andThen nodeSeq andThen PushNode.serializeXML
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * Push Formatted text from the context down to output stream.
   */
  class PushFormattedText(formatter: Context => String) extends PushFromContext(
    formatter andThen ((x:String) => Stream(new EvText(x)))
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * Shortcut to output various objects downstream.
   */
  object > {
    def apply(formatter: Context => String) : PushFormattedText = {
      new PushFormattedText(formatter = formatter)
    }

    def apply(text : String) : PushText = {
      new PushText(text = text)
    }

    def apply(event : XMLEvent) : PushFromContext = new PushFromContext(c => Stream(event))

    def apply(events :Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => events)
  }

  /**
   * Output text downstream.
   */
  class PushText(text: String) extends PushFromContext(x => Stream(new EvText(text))) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * A NameSpaceMatcher checks the namespace based on its qualified name.
   */
  type NameSpaceMatcher = QName => Boolean

  /**
   * This implicit NamespaceMatcher does no check at all on the namespace's qualified name.
   */
  implicit object DefaultNamespaceMatcher extends NameSpaceMatcher {
    def apply(v1: QName) : Boolean = true
  }

  /**
   * An abstract matcher for all tags (starting and ending)
   */
  abstract class EvTagMatcher extends CPSElemMatcher


  /**
   * Matches an EvElemStart
   */
  abstract class EvStartMatcher(nameSpaceMatcher :NameSpaceMatcher) extends EvTagMatcher {
    /**
     * implement this to check whether the Element is the one we are looking for.
     */
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean

    /**
     * Call the testElem method to check the event.
     */
    def apply(event: XMLEvent) : Boolean = {
      event match {
        case EvElemStart(name, attributes) => {
           testElem(name, attributes)
        }
        case _ => false
      }
    }
  }

  /**
   * A matcher that matches EvElemStarts based on their localPart.
   */
  class LocalPartEvStartMatcher(localPart : String)(implicit nameSpaceMatcher :NameSpaceMatcher) extends EvStartMatcher(nameSpaceMatcher) {
    def testElem(name: QName, attributes: Map[QName, String]) =
          localPart.equals(name.localPart) && nameSpaceMatcher(name)
  }

  /**
   * Shortcut to take an opening tag based on the localpart.
   */
  object < {
    
    def apply(evStartMatcher : EvStartMatcher) : ElementMatcherTaker = {
      new ElementMatcherTaker(
          evStartMatcher
        )
    }

  }

  /**
   * This allows for automatic conversion toward an EvStartMatcher from a String.
   */
  implicit def string2EvStartMatcher(s : String)(implicit nameSpaceMatcher :NameSpaceMatcher) : EvStartMatcher = new LocalPartEvStartMatcher(s)(nameSpaceMatcher)
  /**
   * Matches an EvElemEnd
   */
  abstract class EvEndMatcher(nameSpaceMatcher :NameSpaceMatcher) extends EvTagMatcher {
    def testElem(name : QName) : Boolean

    def apply(event: XMLEvent) : Boolean = {
      event match {
        case EvElemEnd(name) => {
           testElem(name)
        }
        case _ => false
      }
    }
  }
  /**
   * A matcher that matches EvElemEnds based on their localPart.
   */
  class LocalPartEvEndMatcher(localPart : String)(implicit nameSpaceMatcher :NameSpaceMatcher) extends EvEndMatcher(nameSpaceMatcher) {
    def testElem(name: QName) = localPart.equals(name.localPart) && nameSpaceMatcher(name)
  }
  /**
   * Shortcut to take a closing tag based on the localpart.
   */
  object </ {

    def apply(matcher : EvEndMatcher) : ElementMatcherTaker = {
      new ElementMatcherTaker(matcher)
    }
    def apply(name : String)(implicit nameSpaceMatcher :NameSpaceMatcher) : ElementMatcherTaker = {
      apply(new LocalPartEvEndMatcher(name)(nameSpaceMatcher))
    }
  }
  /**
   * Shortcut to take text.
   */
  val takeText = new ElementMatcherTaker(new EvTextTypeMatcher())

  /**
   * Shortcut to take space or comment.
   */
  val takeSpace = new ElementMatcherTaker(new SpaceOrCommentMatcher())
  
  /**
   * Util class to build XMLResultStream, save etc...
   */
  object XMLResultStreamUtils {
    /**
     * converts an iterator of XMLEvent to a negative `CPSStream`.
     */
    def convertToCPSStream(input : Iterator[XMLEvent]) : CPSStream =
        (input map ((x :XMLEvent) => (Some(x), false))).toStream

    /**
     * Load a XMLResultStream from an InputStream
     */
    def loadXMLResultStream(in : InputStream) : CPSStream =
        convertToCPSStream(new XMLEventStream(in))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(str : String) : CPSStream =
        loadXMLResultStream(new ByteArrayInputStream(str.getBytes))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(str : =>Stream[Char]) : CPSStream =
        convertToCPSStream(new XMLEventStream(str))

    /**
     * Serialise a XMLResultStream into a XML form.
     */
    def serializeXMLResultStream(evStream : =>CPSStream, writer : Writer) {
      evStream foreach (_._1 match {
                case Some(x : XMLEvent) => x.appendWriter(writer)
                case _ => (new EvComment("EmptyPositive")).appendWriter(writer)
              })
    }

    /**
     * Deserialize from an objectInputStream serialized/binary XMLEvent. 
     */
    def rehydrate(in : ObjectInputStream) : CPSStream = {
      val read = in.readObject
      if (read != null) {
        Stream.cons( (Some((read).asInstanceOf[XMLEvent]), false), rehydrate(in))
      } else {
        Stream.Empty
      }
    }

    /**
     * Serialize to an objectOutputStream serialized/binary XMLEvent.
     */
    def dehydrate(evStream: CPSStream, dataOut: ObjectOutputStream) {
      evStream.foreach(x => {dataOut.writeObject(x._1.get)})
    }
  }
  
}