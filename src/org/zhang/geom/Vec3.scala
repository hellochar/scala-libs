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
   * Returns a Vec3 with a random longitude and latitude, and random magnitude in the range (0, 1).
   */
  def random = {
    //fromSpherical(1, math.random*TWO_PI, (math.random - .5) * PI ) <- this gives a bias at the poles
    var v:Vec3 = null;
    import math.{random => rand}
    do{
      v = Vec3(rand*2-1, rand*2-1, rand*2-1)
    }while(v.mag2 > 1);
    v ofMag rand
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

  /**
  * Returns a vector representing the gravitational force between two bodies located at v1 and v2 (with a gravitational constant of 1).
  */
  def invR2(v1:Vec3, v2:Vec3) = {
    val o = v1 - v2;
    o.normalize / o.mag2
  }

  val magOrdering = Ordering.by((_:Vec3).mag2)

  val ZERO = Vec3(0, 0, 0)
  val X = Vec3(1, 0, 0)
  val Y = Vec3(0, 1, 0)
  val Z = Vec3(0, 0, 1)
}

case class Vec3(x:Float, y:Float, z:Float) extends PartiallyOrdered[Vec3] {

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

  def mag2 = x*x+y*y+z*z
  def mag = sqrt(mag2).toFloat
  def angle = atan2(y, x).toFloat
  def angleZ = -acos(z/mag).toFloat+PI/2

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


  /**
   * Returns the angle between this vector and the given vector. The value returned will always be in the range
   * [0, PI]
   */
  def angleBetween(v:Vec3) = if(isZero || v.isZero) 0f else { //we shouldn't be returning 0f here. It should actually be None
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

  def normalize = if(mag == 0) this else this / mag;

  /**
   * Same as *
   */
  def scale(s:Float) = this * s

  /**
   * Precondition: axis is not the zero vector. <br />
   * Returns this vector rotated theta radians around the axis vector. axis must be of unit length.
   * @param axis Axis of rotation
   * @param theta Radians to rotate
   */
  def rotate(axis:Vec3, theta:Float) = //what if axis is zero? then this formula reduces to this * cos(theta) + 0 + 0 = this * cos(theta), definitely not what we want.
    this * cos(theta) + (axis cross this) * sin(theta) + axis * (axis.dot(this) * (1 - cos(theta)))

  /**
   * Returns true if all components of this vector have less length than epsilon.
   */
  def withinBounds(epsilon:Float) = abs(x) < epsilon && abs(y) < epsilon && abs(z) < epsilon;
  /**
   * Precondition: vX and vY are linearly independent, v is on the plane spanned by vX and vY, vX and vY are orthogonal.
   * Given a vector v on a plane spanned by vX and vY, this method decomposes that vector into its vX and vY components,
   * returning the value in a Vec2.
   * This method is a general bridge to the Vec2 class, and is the "inverse" of Vec2.as3D(vX, vY).
   */
  def as2D(vX:Vec3, vY:Vec3) = {
//    if(math.abs(vX dot vY) > .01f) sys.error("vX not ortho to vY! "+vX+", "+vY+", "+vX.dot(vY)) //todo: get rid of error checking?
//    if(!proj(vX cross vY).withinBounds(.01f)) sys.error("v is not on the plane spanned by vX, vY! "+vX+", "+vY+", "+proj(vX cross vY))
    //since they're orthogonal, we just have to do a projection
    //v = x1 * v1 + x2 * v2; v . v1 = x1 * v1 . v1 + 0 => x1 = v . v1 / (v1 . v1)
    Vec2(dot(vX) / vX.mag2, dot(vY)/ vY.mag2)
  }

  /**
   * Interprets this vector as having coordinates defined relative to the given axis arguments. This method interprets
   * this vector as being described in the coordinate system xAxis, yAxis, zAxis and constructs an equivalent vector.
   * Calling Vec3(1,2,3).onAxes(Vec3.X * 10, Vec3.Y + Vec3.Z, Vec3.Z) will interpret the Vec3(1,2,3) as being composed
   * of the (Vec3.X*10) vector in the x-direction, the (Vec3.Y + Vec3.Z) vector in the y-direction, and the Vec3.Z
   * vector in the Z direction. This operation is the inverse of calling inAxes(xAxis, yAxis, zAxis).
   * Returns xAxis*x + yAxis*y + zAxis*z
   * @param xAxis
   * @param yAxis
   * @param zAxis
   * @return
   */
  def onAxes(xAxis:Vec3, yAxis:Vec3, zAxis:Vec3) = xAxis*x + yAxis*y + zAxis*z;

  /**
   * Returns this vector as represented by the given axis arguments. This method decomposes this vector into its
   * representation in the coordinate system described by xAxis, yAxis, zAxis. Calling Vec3(1,2,3).inAxes(Vec3.X
   * * 10, Vec3.Y + Vec3.Z, Vec3.Z) will decompose Vec3(1,2,3) into a vector storing an amount of (Vec3.X*10) in the x-coordinate,
   * (Vec3.Y + Vec3.Z) in the y-coordinate, and (Vec3.Z) in the z coordinate. This operation is the inverse of calling
   * onAxes(xAxis, yAxis, zAxis)
   * Returns proj(xAxis) + proj(yAxis) + proj(zAxis)
   * @param xAxis
   * @param yAxis
   * @param zAxis
   */
  def inAxes(xAxis:Vec3, yAxis:Vec3, zAxis:Vec3) = proj(xAxis) + proj(yAxis) + proj(zAxis)
}
