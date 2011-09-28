package org.zhang.lib

import processing.core.PApplet
import zhang.Camera

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 6/19/11
* Time: 4:03 AM
*/


/**
 * Stackable trait for PApplet
 */
@deprecated("Not particularly useful and will error out sometimes. Just implement your own camera.")
trait HasCamera extends PApplet {
  protected val cam:Camera = new Camera(this)
}