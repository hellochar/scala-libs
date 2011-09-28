package org.zhang.lib.misc

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
  def fromPolar(radius:Float, theta:Float) = Vec2(radius*cos(theta), radius*sin(theta))

  lazy val magOrdering = Ordering.by((v:Vec2) => v.mag2)

  lazy val ZERO = Vec2(0, 0)
  lazy val X = Vec2(1, 0)
  lazy val Y = Vec2(0, 1)
}

case class Vec2(x:Float, y:Float) extends (Float, Float)(x, y) with PartiallyOrdered[Vec2] {

  implicit def d2f(d:Double) = d.toFloat
  def tryCompareTo[B >: Vec2](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]) = {
    if(that.isInstanceOf[Vec2]) {
      val other = that.asInstanceOf[Vec2]
      if(x < other.x && y < other.y) Some(-1)
      else if(x == other.x && y == other.y) Some(0)
      else if(x > other.x && y > other.y) Some(1)
    }
    None
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

  val + = doToBoth(_+_) _
  val - = doToBoth(_-_) _
  def *(n:Float) = Vec2(x*n, y*n)
  def /(n:Float) = Vec2(x/n, y/n)

  def dot(v:Vec2) = x*v.x+y*v.y

  def unary_- = Vec2(-x, -y)

  def proj(v:Vec2) = v * (dot(v)/(v.mag2))
  def angleBetween(v:Vec2) = math.acos(dot(v)/(v.mag*mag)).toFloat

  /** Consider the Vec2 required to "move" this vector to v. angleTo returns the angle, in radians, of that Vec2.
   */
  def angleTo(v:Vec2) = (v - this) angle

  /** Consider the Vec2 required to "move" this vector to v. distTo returns the magnitude of that Vec2.
   */
  def distTo(v:Vec2) = (v - this) mag

  override def clone = Vec2(x, y)

  def normalize = this / mag;
  /**Aliases for "normalize".
  */
  val normal, norm, normalized = normalize _

  val translate = this.+
  def scale = * _
  def rotate(rad:Float) = {val ct = cos(rad); val st = sin(rad); Vec2((ct*x-st*y).toFloat, (st*x+ct*y).toFloat) }

  /**
  * Returns a vector that is inversely proportional to the square of the distance between this vector and the given vector, scaled with the k factor,
  * and pointing towards the given vector.
  */
  def invR2(k:Float, other:Vec2) = {
      val offset = other - this;
      (offset.normalize * k) / offset.mag2
  }
}