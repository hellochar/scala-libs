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

//  implicit def ii2v2[T, U](i:(T, U))(implicit num1: Numeric[T], num2: Numeric[U]) = {
//    (Float, Float)(num1.toFloat(i._1), num2.toFloat(i._2))
//  }

  def vertex(t:(Float, Float)) { vertex(t._1, t._2) }
  def vertex(t:(Float, Float, Float)) { vertex(t._1, t._2, t._3) }

  def line(end:(Float, Float)) { line(0, 0, end._1, end._2) }
  def line(start:(Float, Float), end:(Float, Float)) { line(start._1, start._2, end._1, end._2) }

  def line(end:(Float, Float, Float)) { line(0, 0, 0, end._1, end._2, end._3) }
  def line(start:(Float, Float, Float), end:(Float, Float, Float)) { line(start._1, start._2, start._3, end._1, end._2, end._3) }

  def ellipse(center: (Float, Float), w: Float, h:Float) { ellipse(center._1, center._2, w, h) }
  def ellipse(center: (Float, Float), d: (Float, Float)) { ellipse(center, d._1, d._2) }
  def point(p:(Float, Float)) { point(p._1, p._2) }
  def rect(corner:(Float, Float), w:Float, h:Float) { rect(corner._1, corner._2, w, h) }
  def triangle(a:(Float, Float), b:(Float, Float), c:(Float, Float)) { triangle(a._1, a._2, b._1, b._2, c._1, c._2) }

  def translate(t:(Float, Float)) { translate(t._1, t._2) }
  def translate(t:(Float, Float, Float)) { translate(t._1, t._2, t._3) }

  /**
   * Draws the list of vec3's in a beginShape/endShape pair
   */
  def lines3(list:Traversable[Vec3], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); }

  /**
   * Draws the list of vec2's in a beginShape/endShape pair
   */
  def lines2(list:Traversable[(Float, Float)], close:Boolean = false) { beginShape(); list.foreach(vertex _); if(close) endShape(CLOSE) else endShape(); } //:( type erasure ftl

  def pointLight(c1:Int, c2:Int, c3:Int, loc:(Float, Float, Float)) { pointLight(c1, c2, c3, loc._1, loc._2, loc._3) }

  /**
   * Rotates the current coordinate system such that the given vector a is aligned with the vector b.
   */
  def rotateAtoB(a:Vec3, b:Vec3) { applyMatrix(P5Util.rotateAtoBMat(a, b)) }

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

//  def fore(r:Iterator[_])(t: => Unit) { r.foreach(_ => t) }

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
   * Generates a color with fields all filled in with random(255).
   */
  def randomColor = color(random(255), random(255), random(255))

}