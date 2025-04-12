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

import scala.collection._
import scala.collection.mutable.Builder

/**
 * The PrefixMap is a prefix dictionary.
 */
class PrefixMap[T]
extends mutable.Map[String, T] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = immutable.Map.empty[Char, PrefixMap[T]]
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T): Unit = {
    withPrefix(s).value = Some(elem)
  }

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
    (for ((chr, m) <- suffixes.iterator;
          (s, v) <- m.iterator) yield (chr +: s, v))

  def addOne(kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def subtractOne(s: String): this.type = { remove(s); this }

  override def empty: PrefixMap[T] = new PrefixMap[T]

  override def clear(): Unit = {
    suffixes = immutable.Map.empty[Char, PrefixMap[T]]
    value = None
  }
}

object PrefixMap {
  def empty[T]: PrefixMap[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m.addOne(kv)
    m
  }

  def newBuilder[T]: Builder[(String, T), PrefixMap[T]] = {
    val b = new mutable.Builder[(String, T), PrefixMap[T]] {
      private val map = empty[T]
      def addOne(elem: (String, T)): this.type = { map.addOne(elem); this }
      def clear(): Unit = map.clear()
      def result(): PrefixMap[T] = map
    }
    b
  }

  implicit def canBuildFrom[T]: mutable.Builder[(String, T), PrefixMap[T]] = newBuilder[T]
}