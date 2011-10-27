package org.zhang.lib.misc

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/23/11
 * Time: 2:43 PM
 */

case class LorenzAttractor(sigma:Float = 10f, phi:Float = 28f, beta:Float = 8/3f, timeStep:Float = 5e-3f) {

  def makePath(start:Vec3) = Stream.iterate(start)(c => c + offset(c)*timeStep)

  def offset(coords:Vec3) = { import coords._; Vec3(sigma*(y-x), x*(phi-z)-y, x*y-beta*z)}

}