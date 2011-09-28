package org.zhang.lib.world

import action.RunawayAction
import org.zhang.lib.world.particle.Particle
import processing.core.PApplet
import org.zhang.lib.misc.Vec2
import collection.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
 * To change this template use File | Settings | File Templates.
 */

//Operations i want are add, remove, and iterator. Optimally I'd say "trait World extends Iterable[Particle] with Addable with Subtractable, but the last two traits have some fucked up self-type stuff that I can't deal with now

//Why is this a concrete class instead of an abstract one?
/**
* The drawPriority may be used to specify the order in which entites will be drawn. All entities with the same drawPriority
* will get drawn in no particular order. For any two numbers x > y, entities with drawPriority = x will get drawn before entities
* with drawPriority = y. This means that entities with lower drawPriorities will get drawn "on top" of entities with higher dP's.
*/
class Entity(val world:World, val drawPriority:Float = 1f) {
  def run() {}
  def update() {}
  def draw(p:PApplet) {}

  //When this entity entered the World.
  protected val start = world.getApplet.millis
  /**
  * The number of milliseconds this Entity has been alive for.
  */
  def lifeTime = world.getApplet.millis - start;
  def remove() { world.synchronized { world -= this } }
}

//Need to refine concept. Should all of the inner entities also be added to the world? What if some entities have locations and others don't?
//trait MultipleEntity(w, dp) extends Entity {
//  def entities:Set[Entity]
//
//  def run = entities foreach(_.run)
//  def update = entities foreach(_.update)
//  def draw(p:PApplet) = entities foreach(_.draw(p))
//}


//trait Runnable extends Entity {def run:Any}
//trait Updateable extends Entity {  def update:Any }
//trait Drawable extends Entity {  def draw(p:PApplet):Any }
trait Location extends Entity {
  def loc:Vec2
  def x = loc.x; def y = loc.y;
}
trait Movable extends Location {
  var loc:Vec2
  def x_=(x1:Float) = loc = new Vec2(x1, y); def y_=(y1:Float) = loc = new Vec2(x, y1);
}
trait Velocity extends Movable {
  var vel:Vec2
  def angle = vel.angle;
  def angle_=(ang:Float) = vel = vel.setAngle(ang)
  abstract override def update = {
    loc += vel * world.timeStep;
    super.update
  }
}
trait Acceleration extends Velocity {
  var acc:Vec2 = _
  abstract override def update = {
    vel += acc * world.timeStep
    super.update
  }
}
trait Forceable extends Acceleration {
  protected var forces = Set[Vec2]()
  def addForce(f:Vec2) = forces += f

  abstract override def update = {
    acc = forces.foldLeft(Vec2())((a:Vec2, b:Vec2) => a + b)
    forces.clear
    super.update
  }
}

trait SpecialBounded extends Entity {
  def whenHitsBounds
}