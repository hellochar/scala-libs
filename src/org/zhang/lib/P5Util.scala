package org.zhang.lib

import processing.core._;
import org.zhang.geom._
import zhang.Camera

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 6/19/11
* Time: 2:10 AM
* Scala utilities for Processing, usually with regards to things in org.zhang. You must have processing (core.jar) as part of your classpath.
* Drawing methods ellipse, line, point, rect, triangle
*/

@deprecated("Use MyPApplet instead")
case class P5Util(p: PApplet) {
  import p._;
//  {
//    val sm = System.getSecurityManager
//    if(sm == null || sm.checkPermission(new RuntimePermission(""))) {
//
//    val classLoader = ClassLoader.getSystemClassLoader
//    try {
//      classLoader.loadClass("processing.core.PVector")
//      classLoader.loadClass("processing.core.PApplet")
//    }catch {
//      case e: ClassNotFoundException => throw new RuntimeException("Cannot find Processing library, check that it's in your classpath!", e)
//    }
//    }
//  }
//  implicit def pvector2vec2(p:PVector) = Vec2(x, y)
//  implicit def vec22pvector(v:Vec2) = new PVector(v.x, v.y)

  @deprecated("Use MyPApplet instead")
  def line(start: Vec2, end: Vec2) = p.line(start.x, start.y, end.x, end.y)
  @deprecated("Use MyPApplet instead")
  def ellipse(center: Vec2, w: Float, h:Float) = p.ellipse(center.x, center.y, w, h)
  @deprecated("Use MyPApplet instead")
  def point(point:Vec2) = p.point(point.x, point.y)
  @deprecated("Use MyPApplet instead")
  def rect(corner:Vec2, w:Float, h:Float) = p.rect(corner.x, corner.y, w, h)
  @deprecated("Use MyPApplet instead")
  def triangle(a:Vec2, b:Vec2, c:Vec2) = p.triangle(a.x, a.y, b.x, b.y, c.x, c.y)

  def randomVector = Vec2(random(width), random(height))

  private def trap8Bits(lshift: Int) = (color: Int) => ((color >> lshift) & 0xFF);
  val red = trap8Bits(16)
  val green = trap8Bits(8)
  val blue = trap8Bits(0)

  def colorDistance(target: Int, arg: Int) = PApplet.dist(0, 0, 0, red(target) - red(arg), green(target) - green(arg), blue(target) - blue(arg));

  def cleanBackground(colorWant: Int = 0xFFFFFFFF, threshold: Float = 15) {
    loadPixels()
    for (i <- 0 until pixels.length) {
      if (colorDistance(colorWant, pixels(i)) < threshold) {
        pixels(i) = colorWant;
      }
    }
    updatePixels()
  }

  def drawGraphAxes(cam:Camera, tickInterval:Int = 1, tickLength:Float = .1f) {
    val rect = cam.getRect;

    p.line(rect.x, 0, rect.x+rect.width, 0); //draw horizontal line
    p.line(0, rect.y, 0, rect.y+rect.height); //draw vertical line

    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).x, cam.model(new PVector(width, height)).x)
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(x <- min until max) {
        p.line(x, -tickLength, x, tickLength)
      }
    }
    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).y, cam.model(new PVector(width, height)).y) //since y downwards is positive, the minimum y is at the topleft corner.
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(y <- min until max) {
        p.line(-tickLength, y, tickLength, y)
      }
    }
  }

}
object P5Util {
  /**
   * Returns a random screen position in the form of a Vec2.
   */
  def randomVector(p:PApplet) = Vec2(p.random(p.width), p.random(p.height))
  @deprecated("Use MyPApplet instead")
  def ellipse(p:PApplet, center: Vec2, w: Float, h:Float) = p.ellipse(center.x, center.y, w, h)

  @deprecated("Use MyPApplet instead")
  def line(p:PApplet, start: Vec2, end: Vec2) = p.line(start.x, start.y, end.x, end.y)

  def drawGraphAxes(p:PApplet, cam:Camera, tickInterval:Float = 1f, tickLength:Float = .1f) {
    val rect = cam.getRect;
    import p._;

    p.line(rect.x, 0, rect.x+rect.width, 0); //draw horizontal line
    p.line(0, rect.y, 0, rect.y+rect.height); //draw vertical line

    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).x, cam.model(new PVector(width, height)).x)
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(x <- Range.Double(min, max, tickInterval)) {
        p.line(x.toFloat, -tickLength, x.toFloat, tickLength)
      }
    }
    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).y, cam.model(new PVector(width, height)).y) //since y downwards is positive, the minimum y is at the topleft corner.
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(y <- Range.Double(min, max, tickInterval)) {
        p.line(-tickLength, y.toFloat, tickLength, y.toFloat)
      }
    }
  }

  /**
   * Returns a rotation matrix that transforms the unit a vector into the unit b vector. This method makes no
   * guarantees about the direction in which the other basis vectors may be pointing other than that they will still
   * be orthogonal.
   * @param a non-zero input vector; doesn't have to be normalized
   * @param b non-zero output vector; doesn't have to be normalized
   * @return A PMatrix3D object that gives the output vector, given the input vector.
   */
  def rotateAtoBMat(a:Vec3, b:Vec3) = { //todo: what if a is zero? what if b is zero?
    val c = (a cross b).normalize; val ang = a angleBetween b;
    val v = (new PMatrix3D);
    v.rotate(ang, c.x, c.y, c.z);
    v;
  }

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
  def rotatePlaneAtoBMat(aOn: Vec3, aNorm: Vec3, bOn: Vec3, bNorm: Vec3) = { //todo: what if any of the vectors are zero?
    val aToBNorms = rotateAtoBMat(aNorm, bNorm); //this transforms aNorm into bNorm (z to Z)
    val aToBOns = rotateAtoBMat(transformed(aOn, aToBNorms), bOn); //this transforms aOn into bOn in the normal transform's coordinates (does both x to N and N to X)
    //transformed(aOn, aToBNorms) gives us the line of nodes, which we then transform again to get to X.

    aToBOns.apply(aToBNorms); //concat the two together to get one matrix that does both transformations
    aToBOns;
  }

  /**
   * Applies the given matrix to the vector
   * @param v vector to transform
   * @param m Matrix describing the transformation
   * @return Application of m onto v
   */
  def transformed(v: Vec3, m: PMatrix3D) =
    Vec3(m.multX(v.x, v.y, v.z), m.multY(v.x, v.y, v.z), m.multZ(v.x, v.y, v.z))

}