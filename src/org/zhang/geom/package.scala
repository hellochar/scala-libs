package org.zhang

import lib.P5Util

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

// This method can and should be replaced with Vec3.rotate; it's kept here for reference
//  /**
//   * Precondition: norm is orthogonal to v.<br />
//   * We imagine the great circle described by the norm vector. v will
//   * be on the great circle. This method will return a Vec3, on the great circle, a distance norm.mag away from
//   * v (using spherical distance, not euclidean). Interpret the magnitude of v to be the radius of the sphere. <br /><br />
//   * This method is the spherical equivalent to adding a velocity vector to a location vector; the location vector is v, and the
//   * velocity vector has magnitude |norm| and direction perpendicular to norm.
//   * @param v Vector on the great circle
//   * @param norm Vector describing the great circle
//   * @return A Vec3 also on the great circle a distance norm.mag away from v, or v itself if norm is the zero vector.
//   */
//  def move(v: Vec3, norm: Vec3) = if(norm.isZero || v.isZero) v else {
//      //    val a = rotateAtoBMat(Vec3.X, v); //so we can treat the X axis as v.
//      //    a.apply(rotateAtoBMat(transformed(Vec3.Z, a), norm));
//      val rot = P5Util.rotatePlaneAtoBMat(Vec3.X, Vec3.Z, v, norm); //todo: get rid of dependency!
//      //using rot we can now imagine v on the x-axis and norm on the z-axis, which reduces moving into a simple cartesian rotation
//      val theta = norm.mag / v.mag;
//      val nx = Vec2.fromPolar(v.mag, theta).xy
//      //if(!a.invert()) sys.error("a didn't invert! it is "+a.get(null))
//      //run nx through a and return it
//      P5Util.transformed(nx, rot)
//    }

  /**
   * Returns the great-circle distance between v1 and v2; the sphere is assumed to have a radius |v1|. This is the
   * spherical analogue to the Euclidean distance (v2 - v1).mag
   * @param v1 One point on the sphere
   * @param v2 Second point on the sphere.
   */
  def distS(v1:Vec3, v2:Vec3) = v1.mag * (v1 angleBetween v2)

}