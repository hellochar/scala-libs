package org.zhang.lib.world.particle

import processing.core.PApplet
import org.zhang.geom.Vec2
import org.zhang.lib.world._
import org.zhang.lib.world.action.Action

class Particle(w:World, l:Vec2, v:Vec2, var actions: Iterable[Action]) extends Entity(w) with Forceable {

  def this(w:World, l:Vec2, v:Vec2) = this(w, l, v, Seq())
  def this(w:World, l:Vec2) = this(w, l, Vec2())
  
  var loc = l;
  var vel = v;
  acc = Vec2()

  override def run = {//in the run phase, each particle does everything it might want to do, which mostly involves putting forces on
    //itself and other particles.
    actions foreach(_(this))
  }

  override def toString = loc.toString
}