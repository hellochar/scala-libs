package org.zhang.geom

import org.scalatest.FunSuite

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/27/11
 * Time: 7:04 PM
 */

class PackageTest extends FunSuite {
  import Mesh._
  test("sphereMesh") {

  }

  test("octahedron") {
    val o = octahedron(100);
    val points = o.points
    assert(points.size == 6);
    assert(points.map(_.mag) == Set(100));
    assert(o.triangles.size == 8)
    assert(Set(Vec3(0, 0, 100), Vec3(0, 0, -100), Vec3(100, 0, 0), Vec3(0, 100, 0), Vec3(-100, 0, 0), Vec3(0, -100, 0)) ==
      points)
    assert(o.edges.size == 12)
  }
}