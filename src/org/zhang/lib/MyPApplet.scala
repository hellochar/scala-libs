package org.zhang
package lib

import geom._
import processing.core._

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 7/15/11
* Time: 1:11 PM
*/

trait MyPApplet extends PApplet with HasMV {
  import PConstants._

  object Vec2Conversion {
    implicit def vec2ToPVector(v2: Vec2) = new PVector(v2.x, v2.y)
    implicit def pVector2Vec2(pv: PVector) = Vec2(pv.x, pv.y)
  }
  object Vec3Conversion {
    implicit def vec3ToPVector(v3: Vec3) = new PVector(v3.x, v3.y, v3.z)
    implicit def pVector2Vec2(pv: PVector) = Vec3(pv.x, pv.y, pv.z)
  }

  //======================================OVERLOADED TUPLE METHODS==============================================
//  implicit def ii2v2[T, U](i:(T, U))(implicit num1: Numeric[T], num2: Numeric[U]) = {
//    Vec2(num1.toFloat(i.x), num2.toFloat(i.y))
//  }

  //--------------2D Methods-------------
  def vertex(t:Vec2) { vertex(t.x, t.y) }

  def line(end:Vec2) { line(0, 0, end.x, end.y) }
  def line(start:Vec2, end:Vec2) { line(start.x, start.y, end.x, end.y) }

  def ellipse(center: Vec2, w: Float, h:Float) { ellipse(center.x, center.y, w, h) }
  def ellipse(center: Vec2, d: Vec2) { ellipse(center, d.x, d.y) }

  def point(p:Vec2) { point(p.x, p.y) }

  def rect(corner:Vec2, w:Float, h:Float) { rect(corner.x, corner.y, w, h) }
  def triangle(a:Vec2, b:Vec2, c:Vec2) { triangle(a.x, a.y, b.x, b.y, c.x, c.y) }

  def translate(t:Vec2) { translate(t.x, t.y) }
  def scale(s:Vec2) { scale(s.x, s.y) }
  /**
   * Draws the list of vec2's in a beginShape/endShape pair
   */
  def lines2(list:Traversable[Vec2], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); }

  //todo: arc, quad, 2D bezier, 2D curve

  //------------------------------------



  //------------3D Methods---------------
  def vertex(t:Vec3) { vertex(t.x, t.y, t.z) }

  def line(end:Vec3) { line(0, 0, 0, end.x, end.y, end.z) }
  def line(start:Vec3, end:Vec3) { line(start.x, start.y, start.z, end.x, end.y, end.z) }

  def point(p:Vec3) { point(p.x, p.y, p.z) }

  def translate(t:Vec3) { translate(t.x, t.y, t.z) }
  def scale(s:Vec3) { translate(s.x, s.y, s.z) }
  def rotate(rad:Float, axis:Vec3) { rotate(rad, axis.x, axis.y, axis.z) }

  /**
   * Draws the list of vec3's in a beginShape/endShape pair
   */
  def lines3(list:Traversable[Vec3], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); }

  /**
   * Draws a bezier curve through the four points specified; the curve will start at anchor1 and end at anchor2, with control points
   * at cntrol1 and cntrol2
   * @param anchor1 First anchor
   * @param cntrol1 First control
   * @param cntrol2 Second control
   * @param anchor2 Second anchor
   */
  def bezier(anchor1:Vec3,
             cntrol1:Vec3,
             cntrol2:Vec3,
             anchor2:Vec3) {
    bezier(
      anchor1.x, anchor1.y, anchor1.z,
      cntrol1.x, cntrol1.y, cntrol1.z,
      cntrol2.x, cntrol2.y, cntrol2.z,
      anchor2.x, anchor2.y, anchor2.z)
  }
  def pointLight(c1:Float, c2:Float, c3:Float, loc:Vec3) { pointLight(c1, c2, c3, loc.x, loc.y, loc.z) }
  def directionalLight(c1:Float, c2:Float, c3:Float, dir:Vec3) { directionalLight(c1, c2, c3, dir.x, dir.y, dir.z) }
  def ambientLight(c1:Float, c2:Float, c3:Float, dir:Vec3) { ambientLight(c1, c2, c3, dir.x, dir.y, dir.z) }
  //------------------------------------


  //====================================================================================================================








  //=======================================UTILITY MATRIX MANIPULATION==================================================
  /**
   * Rotates the current coordinate system such that the given vector a is aligned with the vector b.
   */
  def rotateAtoB(a:Vec3, b:Vec3) { applyMatrix(P5Util.rotateAtoBMat(a, b)) }

  /**
   * Translates by the specified vector and then takes the given action; this method automatically saves the current matrix with push/popMatrix.
   * @param v Vector to translate by
   * @param action Action to take
   * @tparam A
   * @return
   */
  def at[A](v: Vec3)(action: => A) {
    at(v.x, v.y, v.z)(action)
  }

  /**
   * Translates by the specified coordinates and then takes the given action; this method automatically saves the current matrix with push/popMatrix.
   * @param x x coordinate
   * @param y y coordinate
   * @param z z coordinate
   * @param action action to take
   * @tparam A
   * @return
   */
  def at[A](x: Float, y: Float, z: Float)(action: => A) = matrix {
    translate(x, y, z)
    action
  }

  /**
   * Perform the given action surrounded in a push/popMatrix
   * @param action
   * @tparam A
   */
  def matrix[A](action: => A) {
    pushMatrix()
    action
    popMatrix()
  }

  /**
   * Applies the given matrix and then performs action.
   * @param m
   * @param action
   * @tparam A
   * @return
   */
  def matrix[A](m:PMatrix3D)(action: => A) {
    matrix{applyMatrix(m); action}
  }
  //====================================================================================================================









  //================================================MISC DRAWING======================================================
  /**
   * Draws a cylinder with radius baseRad on the XY plane (centered at the origin) and radius endRad on the XY plane lifted by zLen in the positive Z direction.
   * @param precision How accurate the cylinder should look. A precision of 1 should be pixel accurate (with a zoom of 1); a precision of 10 should jump 10 pixels at a time
   */
  def cylinder(baseRad: Float, zLen: Float, endRad: Float, fillEnds: Boolean = true, precision: Float = 2f) {
    val divisions = (2 * PI * ((baseRad + endRad) / 2) / precision).toInt; //the number of partitions you should split the circle into
    beginShape(TRIANGLE_STRIP)
    val angles = (0 to divisions) map {_ * TWO_PI / divisions} //go all the way TO detail so the cylinder closes properly
    angles foreach {
      ang =>
        vertex(Vec2.fromPolar(baseRad, ang))
        vertex(Vec2.fromPolar(endRad, ang).xy + Vec3.Z * zLen)
    }
    endShape(CLOSE)
    if (fillEnds) {
      lines2(angles map (Vec2.fromPolar(baseRad, _))) //Draw the bottom face
      lines3(angles map (Vec2.fromPolar(endRad, _) withZ zLen)) //Draw the top face
    }
  }

  /**
   * Draw a horizontal line at the given y coordinate
   * @param y
   */
  def horiz(y:Int) = line(0, y, width, y)

  /**
   * Draw a vertical line at the given x coordinate
   * @param x
   */
  def vert(x:Int) = line(x, 0, x, height)

  /**
  * Draws a semicircle of radius rad, extending from the positive x axis until the supplied radian angle.
  * If the angle is positive, the arc will go in a clockwise direction from +x to the specified angle.
  * A negative angle will go in a counterclockwise direction from +x to the specified angle.
  * This method accepts angle arguments in the range [-TWO_PI, TWO_PI]. Angles outside the range will be
  * reduced according to the following algorithm:<br />
  * if ang > TWO_PI, take ang % TWO_PI<br/>
  * if ang < -TWO_PI, take -(-ang % TWO_PI)<br/>
  *
  */  
  def angle(ang:Float, rad:Float = 100) {
    val nAng = if(ang > TWO_PI) ang % TWO_PI else if(ang < -TWO_PI) -(-ang % TWO_PI) else ang
    if(nAng < 0) arc(0, 0, rad, rad, nAng, 0)
    else arc(0, 0, rad*2, rad*2, 0, nAng)
  }
  //====================================================================================================================










  //=======================================UTILITY RANDOM GENERATORS====================================================
  /**
   * Generates a random integer in the range [low, high]. Both ends are inclusive.
   */
  def randi(low:Int, high:Int) = random(low, high+1).toInt

  /**
   * Generates a random integer in the range [0, high].
   */
  def randi(high:Int):Int = randi(0, high)

  /**
   * Generates a random integer in the range [0, 1].
   */
  def randi():Int = randi(1)

  /**
   * Generates a color with fields all filled in with random(255).
   */
  def randomColor = color(random(255), random(255), random(255))
  //====================================================================================================================

}