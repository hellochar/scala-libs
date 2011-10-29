package org.zhang

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/6/11
 * Time: 4:05 PM
 */

package object geom {
  val PI = math.Pi.toFloat
  val TWO_PI = PI * 2;

  /**
  * Returns four Triangles created by bisecting the edges of the original triangle (as described at
  * http://paulbourke.net/miscellaneous/sphere_cylinder/)
  */
  def refine(t:Triangle) = {
    import t._;
    val (p1, p2, p3) = (l1.midpoint, l2.midpoint, l3.midpoint)
    Seq(
      Triangle(v1, p1, p3),
      Triangle(p1, v2, p2),
      Triangle(p3, p2, v3),
      Triangle(p1, p2, p3)
    )
  }

  /**
   * This method takes a collection of triangles and outputs those same triangles, but with vertices all of magnitude
   * rad. A pre-condition is that no triangle has two vertices with equivalent angle and angleZ; that is, if one was to draw
   * a line from the origin to each of the triangle's three vertices, none of those lines should be coincident with the other two.
   */
  def spherify(rad:Float, t:Traversable[Triangle]) = {
    t map (_.mapped(_ ofMag rad))
  }

}