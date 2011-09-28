package org.zhang.lib.world.action

import org.zhang.lib.world.particle.Particle
import org.zhang.lib.world.Location

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: Apr 10, 2011
* Time: 1:38:57 AM
* To change this template use File | Settings | File Templates.
*/

abstract class ClosestParticleAction extends Action {
//  private var whoVar:Particle = _
//  def closest = whoVar;

  import org.zhang.lib.world.location2vec2

  def apply(p:Particle) = {
    import p._
    Action.getClosest(world.ofType(classOf[Location]) - p, p) match {
      case Some(c:Particle) => {applyClosest(p, c)}
      case None => {}
    }
    Unit
  }

  def applyClosest(p:Particle, c:Particle)
}















