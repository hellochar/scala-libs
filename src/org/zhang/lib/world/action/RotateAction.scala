package org.zhang.lib.world.action

import org.zhang.lib.world.particle.Particle

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
 * To change this template use File | Settings | File Templates.
 */

case class RotateAction(var atuPerRot:Float = 60/*atu*/) extends ClosestParticleAction {

  def applyClosest(p:Particle, closest:Particle) = {
      val offset = (closest.loc - p.loc); val dist2 = offset.mag2;
//    val velocity = 2*PI*sqrt(dist2) / atuPerRot;
    val velocity2 = (p.vel - closest.vel).mag2
    val force = offset * velocity2 / dist2
    p.addForce(force)
  }
}