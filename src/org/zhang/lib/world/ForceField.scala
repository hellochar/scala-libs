package org.zhang.lib.world

import org.zhang.lib.misc._
import particle.Particle
import processing.core.PApplet
import collection.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
 * To change this template use File | Settings | File Templates.
 */

trait ForceField extends Entity {
  override val drawPriority = 2f;
  
  abstract override def run = {
    affects.foreach(f => f.addForce(forceFor(f)))
    super.run
  }

  def affects: Set[Forceable] = world.ofType(classOf[Forceable])
  def forceFor(p:Forceable): Vec2
}
//This trait is a forcefield that
trait GoTowards extends ForceField with Location {
  def power:Float
  override def hashCode = loc.hashCode << 16 + power.hashCode

  def forceFor(p: Forceable) = {
    val offset = loc - p.loc;
//    val force = (offset * power) / offset.mag2;
//    if(force.mag2 > 100*100) force.setMag(100)
//    else force;
    offset.setMag(power)
  }

}

trait GravitationalField extends Location with ForceField {
  def power:Float

  def forceFor(p: Forceable) = loc.invR2(power, p.loc)
}

case class Drag(w:World, constant:Float = .01f) extends Entity(w) with ForceField {
  override def hashCode = constant.hashCode

  def forceFor(p:Forceable) = p.vel * (-constant)
}