package org.zhang
package lib

import geom._
import processing.core._
import controlP5._

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

  def lines3(list:Traversable[Vec3]) { beginShape(); list.foreach(vertex _); endShape(); }
  def lines2(list:Traversable[Vec2]) { beginShape(); list.foreach(vertex _); endShape(); } //:( type erasure ftl

  def pointLight(c1:Int, c2:Int, c3:Int, loc:(Float, Float, Float)) { pointLight(c1, c2, c3, loc._1, loc._2, loc._3) }

  /**
   * This method rotates the current coordinate system such that the given vector a is aligned with the vector b.
   */
  def rotateAtoB(a:Vec3, b:Vec3) { applyMatrix(rotateAtoBMat(a, b)) }

  def horiz(y:Int) = line(0, y, width, y)
  def vert(x:Int) = line(x, 0, x, height);

  /**
   * Returns a rotation matrix that transforms the a vector into the b vector.
   * @param a Input vector
   * @param b Output vector
   * @return A PMatrix3D object that gives the output vector, given the input vector.
   */
  def rotateAtoBMat(a:Vec3, b:Vec3) = {
    val c = (a cross b).normalize; val ang = a angleBetween b;
    val v = (new PMatrix3D);
    v.rotate(ang, c.x, c.y, c.z);
    v;
  }

  /**
   * Applies the given matrix to the vector
   * @param v vector to transform
   * @param m Matrix describing the transformation
   * @return Application of m onto v
   */
  def transformed(v: Vec3, m: PMatrix3D) =
    Vec3(m.multX(v.x, v.y, v.z), m.multY(v.x, v.y, v.z), m.multZ(v.x, v.y, v.z))

  /**
   * precondition: aOn is orthogonal to aNorm, bOn is orthogonal to bNorm.<br />
   * If we consider a plane A described by a normal vector aNorm, and a direction on plane A
   * described by the vector aOn, and a second plane B described by bNorm and direction by bOn,
   * this method returns a matrix that transforms aNorm into bNorm, aOn into bOn, and (aOn cross aNorm) into (bOn cross bNorm).
   * @param aOn vector on plane A
   * @param aNorm vector pointing normal to plane A
   * @param bOn vector on plane B
   * @param bNorm vector pointing normal to plane B
   */
  def rotatePlaneAtoBMat(aOn: Vec3, aNorm: Vec3, bOn: Vec3, bNorm: Vec3) = {
    val aToBNorms = rotateAtoBMat(aNorm, bNorm); //this transforms aNorm into bNorm (z to Z)
    val aToBOns = rotateAtoBMat(transformed(aOn, aToBNorms), bOn); //this transforms aOn into bOn in the normal transform's coordinates (does both x to N and N to X)
    //transformed(aOn, aToBNorms) gives us the line of nodes, which we then transform again to get to X.

    aToBOns.apply(aToBNorms); //concat the two together to get one matrix that does both transformations
    aToBOns;
  }

  /**
  * Draws a semicircle of radius rad, extending from the positive x axis until the supplied radian angle.
  * This method accepts angle arguments in the range [-TWO_PI, TWO_PI]. Angles outside the range will be
  * reduced according to the following algorithm:<br />
  * if ang > TWO_PI, take ang % TWO_PI<br/>
  * if ang < -TWO_PI, take -(-ang % TWO_PI)<br/>
  *
  */  
  def angle(ang:Float, rad:Float = 100) {
    val nAng = if(ang > TWO_PI) ang % TWO_PI else if(ang < -TWO_PI) -(-ang % TWO_PI) else ang
    if(nAng < 0) arc(0, 0, rad, rad, nAng, 0)
    else arc(0, 0, rad, rad, 0, nAng)
  }

//  def fore(r:Iterator[_])(t: => Unit) { r.foreach(_ => t) }

  def randi(low:Int, high:Int) = random(low, high+1).toInt
  def randi(high:Int):Int = randi(0, high)
  def randi():Int = randi(1)

  def cp5HasMouse(cp5:ControlP5) = {
    def inCI(p:ControllerInterface) = zhang.Methods.isInRange(new PVector(mouseX, mouseY), p.getAbsolutePosition,
      PVector.add(p.getAbsolutePosition, new PVector(p.getWidth, p.getHeight))
    ) //>_< SO MUCH BOILING

    cp5.getControllerList.exists(inCI _);
  }

  def cp5HasFocus(cp5:ControlP5) = cp5.getControllerList.exists(_ match {
    case c: Textfield => c.isFocus
    case _ => false
  })
}