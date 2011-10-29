package org.zhang.geom

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/26/11
 * Time: 6:25 PM
 */

case class Edge(private val set:Set[Vec3]) {
  assert(set.size == 2, "Edge wasn't made with two points but with "+set.size+"!")
  lazy val (v1, v2) = (set.head, set.last)

  def points = set

  def offset = v2 - v1;
  def midpoint = (v1 + v2) scale .5f;

  def isPoint(v:Vec3) = set contains v
  def other(v:Vec3) = if(v == v1) Some(v2) else if(v == v2) Some(v1) else None

  override def toString = set.mkString("Edge(", ", ", ")")
}

object Edge {
  def apply(v1:Vec3, v2:Vec3):Edge = Edge(Set(v1, v2))
}