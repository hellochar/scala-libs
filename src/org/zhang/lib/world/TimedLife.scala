package org.zhang.lib.world

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: Apr 19, 2011
 * Time: 12:48:43 AM
 */

trait TimedLife extends Entity {
  //How many millis this Entity should live for
  val millisAlive:Float

  abstract override def run = {
    if(lifeTime > millisAlive) {
      remove;
    }
    else
      super.run;
  }

  override def remove() {
    outOfTime;
    super.remove
  }

  def outOfTime() {}
}