package org.zhang.lib.world.action

import collection.mutable._
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.{Location, Entity, Movable}

trait Action extends ((Particle) => Any)
object Action {

  /**
  * Returns the closest A to the given point.
  */
  def getClosest[A <: Location](iter: Iterable[A], point:Vec2) = {
    //easy to understand ordering BUT VERY SLOW.
//    if(!iter.isEmpty) Some(iter.min(Ordering.by((r:A) => point.distTo(r.loc))))
//    else None

    //iterative method that should be faster
    if(iter.isEmpty) None
    else Some(
      iter.foldLeft( (null.asInstanceOf[A], -1f) )((leader, candidate) => {
      //leader is a tuple whose _1 is the A and _2 is the distance
      //candidate is the new A
        val distNew = point.distTo(candidate.loc)
        val distOld = leader._2
        if(distNew < distOld || distOld < 0)
          (candidate, distNew)
        else
          leader
      })._1
    )
  }

}