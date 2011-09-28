package org.zhang.lib

import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.action.{Action, RunawayAction}
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.world.World
import zhang.Camera
import processing.core.PApplet
import collection.mutable._

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: Apr 10, 2011
* Time: 1:38:57 AM
*/

trait WorldApplet extends PApplet with HasMV {
  def world:World

  override def draw = world.synchronized { world.step; world.draw; }
}


/**
 * Add this stackable trait to a PApplet to allow "pausing" of the sketch on a key press. The default key to press
 * is 'p'; you may override the "toPress" def to something else. The state of if the sketch is paused is in the "isPaused"
 * variable. While paused, TogglePauseOnKey will not call super.draw. <br>
 * You should call super.keyPressed() in your own sketch.
 */
trait TogglePauseOnKey extends PApplet {
  def toPress:Char = 'p'
  var isPaused = false

  abstract override def keyPressed = {
    if(key == toPress) isPaused = !isPaused
  }

  abstract override def draw = {
    if(!isPaused) super.draw
  }
}

/**
 * Stackable trait for WorldApplet
 */
trait AddParticlesOnMousePress extends WorldApplet {
  
  def newParticle = new Particle(world, mouseVec, Vec2(), Seq(RunawayAction()))
//  def model(screen:Vec2) = {
//    val matrix = getMatrix
//
//    Vec2(modelX(screen.x, screen.y, 0), modelY(screen.x, screen.y, 0))
//  }

//  def screen(model:Vec2) = {
//    Vec2(screenX(model.x, model.y), screenY(model.x, model.y))
//  }

  abstract override def mousePressed() = {super.mousePressed(); world += newParticle; Unit }
}
