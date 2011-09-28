package org.zhang.lib.world.action

import org.zhang.lib.world.particle.Particle
/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
 */

/**
*
*/
case class RunawayAction(var pow:Float = 15) extends ClosestParticleAction {

  def applyClosest(p: Particle, closest:Particle) = {
    //Find the closest particle and run away
    val force = closest.loc.invR2(pow, p.loc)
    if(force.x.isNaN || force.y.isNaN)
      println("got nan!")
    p.addForce(force)
  }

}
