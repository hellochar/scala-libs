package org.zhang.geom

import math._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Sep 20, 2010
 * Time: 2:56:02 PM
 */
object Vec2 {
  implicit def d2f(d:Double) = d.toFloat

  //This creates automatic conversions for Tuple2s of numeric types (ints, floats, doubles, longs)
  implicit def ii2v2[T, U](i:(T, U))(implicit num1: Numeric[T], num2: Numeric[U]) = {
    Vec2(num1.toFloat(i._1), num2.toFloat(i._2))
  }
  def apply():Vec2 = ZERO
  def apply(t: Float):Vec2 = Vec2(t, t)
  def apply(t: (Float, Float)):Vec2 = this(t._1, t._2)

  def random(radMin:Float, radMax:Float):Vec2 = fromPolar(math.random*(radMax - radMin) + radMin, math.random*TWO_PI)
  /**
   * Returns a vector of random magnitude between [0, 1] and random angle.
   */
  def random = fromPolar(math.random, math.random*TWO_PI);

  /**
   * Returns a vector with random magnitude in the range [0, rad] and random angle.
   * @param rad
   * @return
   */
  def random(rad:Float):Vec2 = random * rad

  def fromPolar(radius:Float, theta:Float) = Vec2(radius*cos(theta), radius*sin(theta))

  /**
  * Returns a vector representing the gravitational force between two bodies located at v1 and v2 (with a gravitational constant of 1).
  */
  def invR2(v1:Vec2, v2:Vec2) = {
    val o = v1 - v2;
    o.normalize / o.mag2
  }

  val magOrdering = Ordering.by((v:Vec2) => v.mag2)

  val ZERO = Vec2(0, 0)
  val X = Vec2(1, 0)
  val Y = Vec2(0, 1)
}

//todo: update to reflect changes to vec3
case class Vec2(x:Float, y:Float) extends PartiallyOrdered[Vec2] {

  implicit def d2f(d:Double) = d.toFloat
  def tryCompareTo[B >: Vec2](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]) = {
    if(that.isInstanceOf[Vec2]) {
      val other = that.asInstanceOf[Vec2]
      if(x < other.x && y < other.y) Some(-1)
      else if(x == other.x && y == other.y) Some(0)
      else if(x > other.x && y > other.y) Some(1)
      else None
    }
    else None
  }

  def mag2 = x*x+y*y
  def mag = sqrt(mag2).toFloat
  def angle = atan2(y, x).toFloat

  @deprecated("use ofMag instead")
  def mag_=(m:Float) = normalize * m;
  @deprecated("use ofAngle instead")
  def angle_=(t:Float) = Vec2.fromPolar(mag, t)

  @deprecated("use ofMag instead")
  val setMag = ofMag _
  @deprecated("use ofAngle instead")
  val setAngle = ofAngle _

  def ofMag(m:Float) = normalize * m;
  def ofAngle(rad:Float) = Vec2.fromPolar(mag, rad);

  def doToBoth(f:(Float, Float) => Float)(v:Vec2) = Vec2(f(x, v.x), f(y, v.y))

  def +(v:Vec2) = Vec2(x + v.x, y + v.y)
  def -(v:Vec2) = Vec2(x - v.x, y - v.y)
  def *(n:Float) = Vec2(x*n, y*n)
  def /(n:Float) = Vec2(x/n, y/n)

  def unary_- = Vec2(-x, -y)

  def dot(v:Vec2) = x*v.x+y*v.y

  def proj(v:Vec2) = v * (dot(v)/(v.mag2))
  def cross(v:Vec2) = x*v.y - y*v.x
  def angleBetween(v:Vec2) = math.acos(dot(v)/(v.mag*mag)).toFloat

  /** Consider the Vec2 required to "move" this vector to v. angleTo returns the angle, in radians, of that Vec2.
   */
  def angleTo(v:Vec2) = (v - this).angle

  /** Consider the Vec2 required to "move" this vector to v. distTo returns the magnitude of that Vec2.
   */
  def distTo(v:Vec2) = (v - this).mag

  def isZero = Vec2.ZERO == this

  override def clone = Vec2(x, y)

  def normalize = if(mag == 0) this else this / mag;

  def translate(v:Vec2) = this + v
  def scale(s:Float) = this * s
  def rotate(rad:Float) = {val ct = cos(rad); val st = sin(rad); Vec2((ct*x-st*y).toFloat, (st*x+ct*y).toFloat) }


  def xy = Vec3(x, y, 0)
  def xz = Vec3(x, 0, y)
  def yz = Vec3(0, x, y)

  /**
   * Interprets this Vec2 as a point on the subspace spanned by vX and vY.
   */
  def as3D(vX:Vec3, vY:Vec3) = vX * x + vY * y;

  def withZ(z:Float) = Vec3(x, y, z)
}