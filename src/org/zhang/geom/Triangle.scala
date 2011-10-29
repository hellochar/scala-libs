package org.zhang.geom

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/25/11
 * Time: 7:57 PM
 */

case class Triangle(v1:Vec3, v2:Vec3, v3:Vec3) {
  assert(!(v1 == v2 || v1 == v3 || v2 == v3), "Triangle was made with some equal vertices! "+v1+", "+v2+", "+v3);
  lazy val (l1, l2, l3) = (Edge(v1, v2), Edge(v2, v3), Edge(v3, v1))

  lazy val edges = List(l1, l2, l3);
  lazy val points = List(v1, v2, v3);

  def mapped(m:Vec3 => Vec3):Triangle = Triangle(m(v1), m(v2), m(v3))
}