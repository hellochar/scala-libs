package org.zhang.geom

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/23/11
 * Time: 2:43 PM
 */

/**
 * A simple class to simulate the Lorenz Attractor. Makes use of streams to allow infinitely many (in theory) values.
 */
case class LorenzAttractor(sigma:Float = 10f, phi:Float = 28f, beta:Float = 8/3f, timeStep:Float = 5e-3f) {

  /**
   * Returns the series of states of this lorenz attractor, beginning at the given start state.
   */
  def makePath(start:Vec3) = Stream.iterate(start)(c => c + offset(c)*timeStep)

  /**
   * Returns the velocity (dx/dt, dy/dt, dz/dt) of the given coordinates.
   */
  def offset(coords:Vec3) = { import coords._; Vec3(sigma*(y-x), x*(phi-z)-y, x*y-beta*z)}

}