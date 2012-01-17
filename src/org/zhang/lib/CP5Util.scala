package org.zhang.lib

import controlP5._
import processing.core.{PVector, PApplet}

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/20/11
 * Time: 11:00 PM
 */

object CP5Util {

  def inCP5(x:Float, y:Float, cp5:ControlP5) = {
    def inCI(p:ControllerInterface) = zhang.Methods.isInRange(new PVector(x, y), p.getAbsolutePosition,
      PVector.add(p.getAbsolutePosition, new PVector(p.getWidth, p.getHeight))
    ) //>_< SO MUCH BOILING

    cp5.getControllerList.exists(inCI _);
  }

  def cp5HasFocus(cp5:ControlP5) = cp5.getControllerList.exists(_ match {
    case c: Textfield => c.isFocus
    case _ => false
  })
}