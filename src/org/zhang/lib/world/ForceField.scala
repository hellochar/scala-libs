package org.zhang.lib.world

import org.zhang.geom._
import particle.Particle
import processing.core.PApplet
import collection.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
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
    offset ofMag power
  }

}

trait GravitationalField extends Location with ForceField {
  def power:Float
  /**
  * Returns a vector that is inversely proportional to the square of the distance between this vector and the given vector, scaled with the k factor,
  * and pointing towards the given vector.
  */
  def invR2(loc:Vec2, other:Vec2) = {
      val offset = other - loc;
      offset.normalize / offset.mag2
  }
  
  def forceFor(p: Forceable) = invR2(loc, p.loc) * power
}

case class Drag(w:World, constant:Float = .01f) extends Entity(w) with ForceField {
  override def hashCode = constant.hashCode

  def forceFor(p:Forceable) = p.vel * (-constant)
}