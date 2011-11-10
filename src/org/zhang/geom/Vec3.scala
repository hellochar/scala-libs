package org.zhang.geom

import math._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/11/11
 * Time: 12:21 PM
 */
object Vec3 {
  implicit private def d2f(d:Double) = d.toFloat

  //This creates automatic conversions for Tuple3s of numeric types (ints, floats, doubles, longs)
  implicit def ii2v2[T, U, V](i:(T, U, V))(implicit num1: Numeric[T], num2: Numeric[U], num3: Numeric[V]) = {
    Vec3(num1.toFloat(i._1), num2.toFloat(i._2), num3.toFloat(i._3))
  }

  def apply():Vec3 = ZERO
  def apply(t: Float):Vec3 = Vec3(t, t, t)
  def apply(t: (Float, Float, Float)):Vec3 = Vec3(t._1, t._2, t._3)

  /**
   * Returns a Vec3 with a random longitude and latitude on the unit sphere.
   */
  def random = {
    //fromSpherical(1, math.random*TWO_PI, (math.random - .5) * PI ) <- this gives a bias at the poles
    var v:Vec3 = null;
    do{
      import math.{random => rand}
      v = Vec3(rand*2-1, rand*2-1, rand*2-1)
    }while(v.mag2 > 1);
    v normalize
  }
  /**
  * Create a Vec3 from the given spherical coordinates.
  * r is the radius, ranged [0 to infinity)
  * t is the azimuth angle (synonymous with theta in Vec2), usually ranged [-PI to PI] or [0 to TWO_PI]
  * angleZ is the elevation angle (also called latitude), ranged [-PI/2 (for the negative Z axis) to PI/2 (for the positive Z axis)]
  */
  def fromSpherical(r:Float, t:Float, angleZ:Float) = Vec3(r*cos(t)*cos(angleZ), r*sin(t)*cos(angleZ), r*sin(angleZ))

  @deprecated("Use fromSpherical(r, t, angleZ) instead")
  def fromPolar(r:Float, t:Float, angleZ:Float) = fromSpherical(r, t, angleZ)

  lazy val magOrdering = Ordering.by((v:Vec3) => v.mag2)

  lazy val ZERO = Vec3(0, 0, 0)
  lazy val X = Vec3(1, 0, 0)
  lazy val Y = Vec3(0, 1, 0)
  lazy val Z = Vec3(0, 0, 1)
}

case class Vec3(x:Float, y:Float, z:Float) extends (Float, Float, Float)(x,y,z) with PartiallyOrdered[Vec3] {

  implicit private def d2f(d:Double) = d.toFloat
  def tryCompareTo[B >: Vec3](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]) =
    if(that.isInstanceOf[Vec3]) {
      val other = that.asInstanceOf[Vec3]
      if(x < other.x && y < other.y && z < other.z) Some(-1)
      else if(x == other.x && y == other.y && z == other.z) Some(0)
      else if(x > other.x && y > other.y && z > other.z) Some(1)
      else None
    }
    else None

  lazy val mag2 = x*x+y*y+z*z
  lazy val mag = sqrt(mag2).toFloat
  lazy val angle = atan2(y, x).toFloat
  lazy val angleZ = atan2(z, mag).toFloat

  /**
   * If this vector is the zero vector, this method returns itself. Otherwise, returns a vector in the same
   * direction as this vector with the given magnitude.
   */
  def ofMag(m:Float) = normalize * m;
  def ofAngle(t:Float) = Vec3.fromSpherical(mag, t, angleZ)
  def ofAngleZ(ang:Float) = Vec3.fromSpherical(mag, angle, ang)

  def doToAll(f:(Float, Float) => Float)(v:Vec3) = Vec3(f(x, v.x), f(y, v.y), f(z, v.z))

  def +(v:Vec3) = Vec3(x + v.x, y + v.y, z + v.z)
  def -(v:Vec3) = Vec3(x - v.x, y - v.y, z - v.z)
  def *(n:Float) = Vec3(x*n, y*n, z*n)
  def /(n:Float) = Vec3(x/n, y/n, z/n)
  def unary_- = Vec3(-x, -y, -z)

  def dot(v:Vec3) = x*v.x+y*v.y+z*v.z

  /**
   * If either this vector or v are the zero vector, this method returns the zero vector. Otherwise, we
   * project this Vec3 onto the given vector v. The resulting vector will point in v's
   * direction.
   * @param v Vec3 to project myself onto.
   */
  def proj(v:Vec3) = if(isZero || v.isZero) Vec3.ZERO else (v * (dot(v)/(v.mag2)))

  /**
   * Precondition: norm is not the zero vector.
   * This projects v onto the plane described by the norm.
   */
  def projPlane(norm:Vec3) = this - (this proj norm)
  def angleBetween(v:Vec3) = if(isZero || v.isZero) 0f else {
    /* Floating point rounding errors can accumulate and make the numerator greater than the denominator
     * which then makes math.acos return NaN. (e.g. this dot v = 100.0, this.mag = .9999994, b.mag = 100.0).
     * If the fraction is ever greater than 1, just assume that the division actually equals one and return 0.
     */
    val d = dot(v) / (v.mag * mag);
    if(abs(d) > 1) 0f;
    else math.acos(d).toFloat
  }
  def cross(other:Vec3) = Vec3(y*other.z - other.y*z, - (x*other.z - other.x*z),  x*other.y - other.x*y)

  /** Consider the Vec3 required to "move" this vector to v. angleTo returns the azimuth angle, in radians, of that Vec3.
   */
  def angleTo(v:Vec3) = (v - this).angle
  /** Consider the Vec3 required to "move" this vector to v. angleZTo returns the elevation angle, in radians, of that Vec3.
   */
  def angleZTo(v:Vec3) = (v - this).angleZ
  /** Consider the Vec3 required to "move" this vector to v. distTo returns the magnitude of that Vec3.
   */
  def distTo(v:Vec3) = (v - this).mag

  def isZero = Vec3.ZERO == this

  override def clone = Vec3(x, y, z)

  /**Alias for "normalize".
  */
  def normalize = if(mag == 0) this else this / mag;

  /**
   * Same as +
   */
  def translate(v:Vec3) = this + v

  /**
   * Same as *
   */
  def scale(s:Float) = this * s



//  def rotate(t:Float) = {val ct = cos(t); val st = sin(t); Vec2(ct*x-st*y, st*x+ct*y) }

  /**
   * Returns true if all components of this vector have less length than epsilon.
   */
  def withinBounds(epsilon:Float) = abs(x) < epsilon && abs(y) < epsilon && abs(z) < epsilon;
}
