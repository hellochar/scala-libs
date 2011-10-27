package org.zhang.lib

import misc.Vec2
import processing.core.PApplet

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 7/8/11
* Time: 2:26 PM
*/

trait HasMV extends PApplet {
  def mouseVec = Vec2(mouseX, mouseY)
}