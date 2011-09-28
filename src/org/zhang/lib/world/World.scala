package org.zhang.lib.world

import collection.mutable._
import action.RunawayAction
import org.zhang.lib.world.particle.Particle
import processing.core.PApplet
import org.zhang.lib.misc.Vec2

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 10, 2011
 * Time: 1:38:57 AM
 * To change this template use File | Settings | File Templates.
 */

//Operations i want are add, remove, and iterator. Optimally I'd say "trait World extends Iterable[Particle] with Addable with Subtractable, but the last two traits have some fucked up self-type stuff that I can't deal with now

trait World extends Set[Entity] {
  val getApplet:PApplet
  
//  val forceFields = collection.mutable.Buffer[ForceField]()
  var timeStep:Float = .1f //the units are "arbitrary time units; atu"; frameRate * atu ~= 1 second.
  private var timeVar:Float = _
  def time = timeVar
  private val priorityQueue:PriorityQueue[Float] = new PriorityQueue[Float]()
  private val queueMapping:Map[Float, Set[Entity]] = Map()
//  private var priorityQueue = new PriorityQueue[Entity]()(Ordering.by((e:Entity) => e.drawPriority)) //lower means you'll get drawn last => you're on top
//
//  abstract override def +=(elem: Entity): this.type = {
//    priorityQueue += elem;
//    super.+=(elem)
//  }
//  abstract override def -=(elem: Entity): this.type = {
//    priorityQueue = priorityQueue.filter(_ != elem); //priorityqueues don't have a -= operator for some reason
//    super.-=(elem);
//  }

  private def setForPriority(dp:Float) = queueMapping.getOrElseUpdate(dp, Set())//priorityQueue.find(_._1 == dp) match { case Some((a, b)) => b; case None => null; }

  abstract override def +=(elem: Entity): this.type = {
    val dp = elem.drawPriority
    if(!queueMapping.contains(dp)) priorityQueue += dp

    setForPriority(dp) += elem;
    super.+=(elem)
    this
  }
  abstract override def -=(elem: Entity): this.type = {
    setForPriority(elem.drawPriority) -= elem;
    super.-=(elem);
    this
  }

  def step():Unit = step(1)
  def step(times:Int) = for(i <- 1 to times) {run; update; timeVar += timeStep;}
  private def doEach(f: Entity => Unit) = foreach(f(_))
  def run() = doEach(_.run)
  def update() = doEach(_.update)
  def draw() {
    //i'd like to do this: drawRequests foreach(_.draw(getApplet)), but ColoredWorld won't do shit.
//    var (particles, others) = drawRequests partition(_.isInstanceOf[Particle])
//    getApplet.pushStyle
//    others foreach(_.draw(getApplet)) //draw others first so the particles get drawn on top
//    getApplet.popStyle

//    particles foreach(_.draw(getApplet))
//    val (particles, others) = partition(_.isInstanceOf[Particle])
//    getApplet.pushStyle; others foreach(_.draw(getApplet)); getApplet.popStyle
//    particles foreach(_.draw(getApplet))

    priorityQueue.foreach(queueMapping(_).foreach(_.draw(getApplet)))
  }

  def ofType[Ents <: Entity, Type <: Ents](c: Class[Type], set:Set[Ents] = this):Set[Type] = //set collect{case t: Type => t} - something wrong here
                                                                                             set.filter(k => c.isAssignableFrom(k.getClass)).asInstanceOf[Set[Type]]

  /**
   * Returns all Entites with Locations in the given circle. Has a very slow O(n) time naive implementation.
   */
  def inCircle[Locs <: Location](center:Vec2, radius:Float, s:Set[Locs] = ofType(classOf[Movable])) = s.filter(_.loc.distTo(center) < radius)

  /**
   * Returns all particles in the given rectangle. Has a slow O(n) time implementation.
   */
  def inRect[Locs <: Location](topLeft:Vec2, dim:Vec2, s:Set[Locs] = ofType(classOf[Movable])) = {
    s.filter(p => {
      p.x > topLeft.x &&       p.y > topLeft.y         &&
      p.x < topLeft.x+dim.x && p.y < topLeft.y + dim.y})
  }

}


class HashWorld(val getApplet:PApplet, tstep:Float = .1f) extends HashSet[Entity] with World {
  timeStep = tstep
}




//class BinnedWorld(applet:PApplet, maxWidth:Int, maxHeight:Int, binsPerDim:Int) extends World {
//
//  val bins: Array[Array[HashSet[Particle]]] = (1 to binsPerDim).toArray.map(i =>
//                                                  (1 to binsPerDim).toArray.map(j => new HashSet[Particle]() ) )
//  def iterator:Iterator[Particle] = {
//    val iter = bins.map(_.iterator) //turns the top level arrays into their respective iterators, returning an Array[Iterator[HashSet[Particle]]
//    val l = iter.reduceLeft(_ ++ _) //appends all of the arrays' iterators together, returning one big Iterator[HashSet[Particle]]
//    val m = l.map(_.iterator) //turns all of the hashsets into their respective iterators, returning an Iterator[Iterator[Particle]]
//    m.reduceLeft(_ ++ _) //appends all of the hashsets' iterators together, returning one really big Iterator[Particle]
//  }
//
//  def getApplet = applet
//
//}

trait BoundedWorld extends World {
  override abstract def update = {
    super.update
    ofType(classOf[Velocity]).filter(v => !inBounds(v.loc)).foreach(_ match {
      case k: SpecialBounded => k.whenHitsBounds
      case v => defaultHitsBounds(v)
    })
  }

  def inBounds(v:Vec2) = !(v.x < 0 || v.x > getApplet.width || v.y < 0 || v.y > getApplet.height)

  def defaultHitsBounds(v:Velocity) {
    if(v.x < 0 || v.x > getApplet.width) {v.x_=(PApplet.constrain(v.x, 0, getApplet.width)); v.vel = Vec2(-v.vel.x, v.vel.y);}
    if(v.y < 0 || v.y > getApplet.height) {v.y = PApplet.constrain(v.y, 0, getApplet.height); v.vel = Vec2(v.vel.x, -v.vel.y);}
  }
}