package org.zhang.lib

import processing.core._, PApplet._, PConstants._
import org.zhang.geom._
import zhang.Camera

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 6/19/11
* Time: 2:10 AM
* Scala utilities for Processing, usually with regards to things in this library. You must have processing (core.jar) as part of your classpath.
* Has implicit defs for pvector <=> vec2, as well as drawing methods ellipse, line, point, rect, triangle
*/

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
    loadPixels
    for (i <- 0 until pixels.length) {
      if (colorDistance(colorWant, pixels(i)) < threshold) {
        pixels(i) = colorWant;
      }
    }
    updatePixels
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
}