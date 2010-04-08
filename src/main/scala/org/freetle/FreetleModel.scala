package org.freetle

import util.XMLEvent

/**
 * Created by IntelliJ IDEA.
 * User: LBRUAND
 * Date: 8 avr. 2010
 * Time: 15:22:12
 * To change this template use File | Settings | File Templates.
 */

trait FreetleModel[Context] {
  // ============ Model =====================
  type event = XMLEvent

  abstract class TransformResult(val sub : event, val context : Option[Context]) {
    def toTail() : TransformResult = {
      this match {
        case Result(sub, context) => new Tail(sub, context)
        case _ => this
      }
    }
    def toResult() : TransformResult = {
      this match {
        case Tail(sub, context) => new Result(sub, context)
        case _ => this
      }
    }
  }
  case class Result(override val sub : event, override val context : Option[Context])
          extends TransformResult(sub : event, context : Option[Context])
  case class Tail(override val sub : event, override val context : Option[Context])
          extends TransformResult(sub : event, context : Option[Context])

    // TODO Find a way to add a constraint on the Stream. (Useful ? )
    //		The constraint being : We have Results until a certain rank and then we have Tails.
  type XMLResultStream = Stream[TransformResult]
    // ============ End of Model ==============

}
