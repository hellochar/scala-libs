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

  //======================================OVERLOADED TUPLE METHODS==============================================
//  implicit def ii2v2[T, U](i:(T, U))(implicit num1: Numeric[T], num2: Numeric[U]) = {
//    (Float, Float)(num1.toFloat(i._1), num2.toFloat(i._2))
//  }

  //--------------2D Methods-------------
  def vertex(t:(Float, Float)) { vertex(t._1, t._2) }

  def line(end:(Float, Float)) { line(0, 0, end._1, end._2) }
  def line(start:(Float, Float), end:(Float, Float)) { line(start._1, start._2, end._1, end._2) }

  def ellipse(center: (Float, Float), w: Float, h:Float) { ellipse(center._1, center._2, w, h) }
  def ellipse(center: (Float, Float), d: (Float, Float)) { ellipse(center, d._1, d._2) }

  def point(p:(Float, Float)) { point(p._1, p._2) }

  def rect(corner:(Float, Float), w:Float, h:Float) { rect(corner._1, corner._2, w, h) }
  def triangle(a:(Float, Float), b:(Float, Float), c:(Float, Float)) { triangle(a._1, a._2, b._1, b._2, c._1, c._2) }

  def translate(t:(Float, Float)) { translate(t._1, t._2) }
  def scale(s:(Float, Float)) { scale(s._1, s._2) }
  /**
   * Draws the list of vec2's in a beginShape/endShape pair
   */
  def lines2(list:Traversable[(Float, Float)], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); }

  //todo: arc, quad, 2D bezier, 2D curve

  //------------------------------------



  //------------3D Methods---------------
  def vertex(t:(Float, Float, Float)) { vertex(t._1, t._2, t._3) }

  def line(end:(Float, Float, Float)) { line(0, 0, 0, end._1, end._2, end._3) }
  def line(start:(Float, Float, Float), end:(Float, Float, Float)) { line(start._1, start._2, start._3, end._1, end._2, end._3) }

  def point(p:(Float, Float, Float)) { point(p._1, p._2, p._3) }

  def translate(t:(Float, Float, Float)) { translate(t._1, t._2, t._3) }
  def scale(s:(Float, Float, Float)) { translate(s._1, s._2, s._3) }
  def rotate(rad:Float, axis:(Float, Float, Float)) { rotate(rad, axis._1, axis._2, axis._3) }

  /**
   * Draws the list of vec3's in a beginShape/endShape pair
   */
  def lines3(list:Traversable[(Float, Float, Float)], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); }

  /**
   * Draws a bezier curve through the four points specified; the curve will start at anchor1 and end at anchor2, with control points
   * at cntrol1 and cntrol2
   * @param anchor1 First anchor
   * @param cntrol1 First control
   * @param cntrol2 Second control
   * @param anchor2 Second anchor
   */
  def bezier(anchor1:(Float, Float, Float),
             cntrol1:(Float, Float, Float),
             cntrol2:(Float, Float, Float),
             anchor2:(Float, Float, Float)) {
    bezier(
      anchor1._1, anchor1._2, anchor1._3,
      cntrol1._1, cntrol1._2, cntrol1._3,
      cntrol2._1, cntrol2._2, cntrol2._3,
      anchor2._1, anchor2._2, anchor2._3)
  }
  def pointLight(c1:Float, c2:Float, c3:Float, loc:(Float, Float, Float)) { pointLight(c1, c2, c3, loc._1, loc._2, loc._3) }
  def directionalLight(c1:Float, c2:Float, c3:Float, dir:(Float, Float, Float)) { directionalLight(c1, c2, c3, dir._1, dir._2, dir._3) }
  def ambientLight(c1:Float, c2:Float, c3:Float, dir:(Float, Float, Float)) { ambientLight(c1, c2, c3, dir._1, dir._2, dir._3) }
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
  def at[A](v: (Float, Float, Float))(action: => A) {
    at(v._1, v._2, v._3)(action)
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