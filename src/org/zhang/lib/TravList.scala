package org.zhang.lib

import collection.{SeqProxy, SeqLike}

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/9/11
 * Time: 8:54 PM
 */

class TravList[+A](list:Seq[A], initIndex:Int = 0) extends SeqProxy[A] {
//  def this(eles:A*) = this(Seq(eles:_*)) //Doesn't work because the interpreter will see any attempt to instantiate a TravList as being an invocation of this constructor
// so things like new TravList(Seq("hi", "hello", "ASDF")) return a TravList[Seq[String]] of length 1
  val self = list
  var index = initIndex
  def right() { index = (index+1)%length } //increment the index counter (with wrapping)
  def left() { index = ((i:Int) => if(i < 0) i + list.length else i)(index-1) } //decrement the index counter (with wrapping)
  def item = list(index) //get the item at the current index

  def apply() = item //same as item
  def -- { left() } //same as left
  def ++ { right() } //same as right

  def move[O](v:O, leftV:O, rightV:O) {if(v == leftV) left() else if(v == rightV) right(); }
}