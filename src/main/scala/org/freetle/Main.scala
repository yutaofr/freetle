package org.freetle


import scala._
import scala.Stream
import scala.io.Source
import org.freetle.transform._
import util.{StreamSource, EvElemStart, XMLEventStream}

object Main {
  

	val el = Stream.concat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<body>\n".toList.toStream,
                         Stream.concat(Stream.make(10000000, "<msg/>\n".toList.toStream)),
                         "</body>\n".toList.toStream).elements
    val src = StreamSource.fromIterator(el)
    
    def main(args: Array[String]) {
      //println(el.foldLeft(0)( (x,y) => x ^ y.hashCode()))
     
      
      /*val id =  new IdTransform()      
      println(id(Stream.fromIterator( new XMLEventStream(src) map (Tail(_)) ) ).foldLeft(0)( (x,y) => x ^ y.hashCode()))*/
      //idResult.foreach(x => println(x))
      val depthTest = 100
    	val i = Stream.make(depthTest, new EvElemStart("p", "message", null, null)) map (Tail(_))
		val t = new RepeatUntilNoResultOperator(new TakeStartElement("message"))
		val r = t(i)
        println(r)
        println ( r.filter(x => x match {
		  case Result(_) => true
		  case _ => false
		}).length)
    }
}
                   
