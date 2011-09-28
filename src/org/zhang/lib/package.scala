package org.zhang

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/6/11
 * Time: 4:05 PM
 */

package object lib {

  /**
  * Returns a memoized version of the given function that does naive caching of key-value pairs.
  */
  def memoize[A, B](f: A => B) = {
    val cache = collection.mutable.WeakHashMap[A, B]()
    ((a: A) => cache.getOrElseUpdate(a, f(a)), cache)
  }

  def interleave[A](s1:Stream[A], s2:Stream[A]):Stream[A] = Stream.cons(s1.head, Stream.cons(s2.head, interleave(s1.tail, s2.tail)))
  def partialSum[T](s:Stream[T])(implicit n:Numeric[T]) = {var k = n.zero; s.map(i => {k = n.plus(k, i); k})}
  def partialDif[T](s:Stream[T])(implicit n:Numeric[T]) = s.sliding(2).toStream.map{ case a #:: b #:: Stream.Empty => n.minus(b, a) }

  //selects a random element from the iterable.
  def random[A](e:Iterable[A]) = e.view.drop((math.random*e.size).toInt).first

  def powerset[X](xs: Set[X]) = (Set(Set.empty[X]) /: xs) ((xss, x) => xss ++ xss.map(_ + x))


  /**
  * Times the evaluation of a method using System.nanoTime and returns (result, nanoseconds)
  */
   def time[A](f: => A) = {
    val before = System.nanoTime()
//    f()
//    val after = System.nanoTime()
    (f, System.nanoTime() - before) //hmmm....?
  }

  /**
  * Evaulates the function some number of times and returns a seq of the nanoseconds spent evaluating each time.
  */
  def repeat[A](f: => A, times:Int = 1) = for(i <- 0 until times) yield time(f)._2

}