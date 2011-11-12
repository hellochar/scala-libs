package org.zhang

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/6/11
 * Time: 4:05 PM
 */

package object lib {

  /**
  * Creates a naively cached memoized version of the given function, and then returns tuple (function, cache)
   * @return The tuple (Memoized function, memoized function's cache)
  */
  def memoize[A, B](f: A => B) = {
    val cache = collection.mutable.WeakHashMap[A, B]() //WeakHashMap?
    ((a: A) => cache.getOrElseUpdate(a, f(a)), cache)
  }

  def interleave[A](s1:Stream[A], s2:Stream[A]):Stream[A] = Stream.cons(s1.head, Stream.cons(s2.head, interleave(s1.tail, s2.tail)))
  def partialSum[T](s:Stream[T])(implicit n:Numeric[T]) = {var k = n.zero; val m = new AnyRef(); s.map(i => m.synchronized{k = n.plus(k, i); k})} //!not thread safe!
  def partialDif[T](s:Stream[T])(implicit n:Numeric[T]) = s.sliding(2).toStream.map{ case a #:: b #:: Stream.Empty => n.minus(b, a) }

  /**
   * If s = [1, 2, 3, 4, 5, ...], <br />
   * then partials(s) = [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], ...]
   */
  def partials[A](s:Stream[A]) = {
    def build(start:Stream[A], rest:Stream[A]):Stream[Stream[A]] = rest match { //todo: maybe a way to make this faster? matching is slow
      case a #:: b => {
        val s = start :+ a;
        Stream.cons(s, build(s, b))
      }
      case Stream.Empty => Stream.Empty
    }
    build(Stream.empty, s)
  }

  def average[A](s:collection.GenTraversableOnce[A])(implicit n:Fractional[A]) = n.div(s.sum, n.fromInt(s.size));
  def average(s:collection.GenTraversableOnce[Int]) = s.sum / s.size;
  
  //selects a random element from the iterable.
  def random[A](e:Iterable[A]) = e.view.drop((math.random*e.size).toInt).head

  def powerset[X](xs: Set[X]) = (Set(Set.empty[X]) /: xs) ((xss, x) => xss ++ xss.map(_ + x))
  
  def withString[T0, R](str:String, func:T0 => R) = new ((T0) => R) {
    override def toString() = str
    def apply(t0: T0) = func(t0)
  }
  def withString[T0, T1, R](str:String, func:(T0, T1) => R) = new ((T0, T1) => R) {
    override def toString() = str
    def apply(t0: T0, t1: T1) = func(t0, t1)
  }
  def withString[T0, T1, T2, R](str:String, func:(T0, T1, T2) => R) = new ((T0, T1, T2) => R) {
    override def toString() = str
    def apply(t0: T0, t1: T1, t2: T2) = func(t0, t1, t2)
  }

  /**
  * Times the evaluation of a method using System.nanoTime and returns (result, nanoseconds)
  */
   def time[A](f: => A) = {
    val before = System.nanoTime()
    val r = f //possible performance hit here
    val elapsed = System.nanoTime() - before
    (r, elapsed) //hmmm....?
  }

  /**
  * Evaulates the function some number of times and returns a seq of the nanoseconds spent evaluating each time.
  */
  def times[A](f: => A, times:Int = 1) = for(i <- 0 until times) yield time(f)._2

  def fastForeach[T, U](seq:Seq[T], func:T => U) {
    var i = 0;
    while(i < seq.length) {
      func(seq(i));
      i += 1;
    }
  }

}