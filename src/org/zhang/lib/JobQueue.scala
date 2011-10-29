package org.zhang.lib

import collection.immutable.Queue

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/13/11
 * Time: 10:05 PM
 */
object JobQueue {
  val VERSION = "0.1.1.1"

  private type Job = Any
  private type MyQueue = Queue[() => Job]
  private var queue:MyQueue = Queue();

  def addJob(job: => Any) { queue = queue enqueue(() => job) }
  def work() {val (job, queueTemp) = (queue dequeue); queue = queueTemp; job.apply}
  def workAll() { queue foreach(_()); queue = Queue() }
  def hasJobs = !queue.isEmpty

}