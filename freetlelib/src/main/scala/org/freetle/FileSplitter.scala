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

import annotation.tailrec
import java.io.Writer
import util.{EvComment, XMLEvent}
import javax.xml.stream.{XMLStreamWriter, XMLOutputFactory}
import com.ctc.wstx.stax.WstxOutputFactory

/**
 * This trait can be used to cut a single stream into multiple files.
 * It matches repeatedly `fileMatcher` and each chunk thus matched can be
 * written into an writer created using `writerConstructor`.
 */

trait FileSplitter[Context] extends CPSXMLModel[Context] {
  val fileMatcher : ChainedTransformRoot = (((takeSpace.*)  ~ <("File")) -> drop) ~  new DeepFilterUntil() ~
                                            (</("File") -> drop )
  /**
   * Serialise a XMLResultStream into a XML form.
   */
  final def serializeXMLResultStream(evStream : =>CPSStream,
                               writerConstructor : (Int, Writer) => Writer,
                               occurrence : Int,
                               writerInput : Option[Writer],
                               context : Context) : Unit = {

    val transformation = fileMatcher(new CFilterIdentity(), new CFilterIdentity())
    var currentStream = evStream
    var index = occurrence
    var carryOnWhileLoop = true
    var outWriter : Option[Writer] = writerInput

    while (carryOnWhileLoop) {
      currentStream = transformation(CPSStreamHelperMethods.turnToTail(currentStream), context)

      var read: Boolean = false
      var xmlStreamWriter : Option[XMLStreamWriter] = None
      try {
        outWriter = Some(writerConstructor(index, outWriter.orNull))
        val outputFactory : WstxOutputFactory = new WstxOutputFactory()
        outputFactory.configureForSpeed()
        xmlStreamWriter = Some(outputFactory.createXMLStreamWriter(outWriter.get))
        while (!currentStream.isEmpty && currentStream.head._2) {

          currentStream.head._1 match {
            case Some(x: XMLEvent) => {
              read = true
              x.appendTo(xmlStreamWriter.get)
            }
            case None => ()
          }
          currentStream = currentStream.tail
        }
        if (read) {
          xmlStreamWriter.foreach(_.close())
          xmlStreamWriter = None
        }
        outWriter.foreach(_.flush())
      }
      catch {
        case e : Throwable => {
          if (!read || xmlStreamWriter.isEmpty) {
            outWriter.foreach(_.close())
          } else {
            xmlStreamWriter.foreach(_.close())
          }
          throw e
        }
      }
      finally {
        if (!read) {
          outWriter.foreach(_.close())
        }
      }
      carryOnWhileLoop = (!currentStream.isEmpty && read)
      if (carryOnWhileLoop) { // Go ahead
        index += 1
      }
    }



  }

  @deprecated("Use serializeXMLResultStream(..., writerInput = Option[Writer], context)", "1.4")
  final def serializeXMLResultStream(evStream : =>CPSStream,
                               writerConstructor : (Int, Writer) => Writer,
                               occurrence : Int = 0,
                               writerInput : Writer = null,
                               context : Context) : Unit =
    serializeXMLResultStream(evStream, writerConstructor, occurrence, Option(writerInput), context)
}
