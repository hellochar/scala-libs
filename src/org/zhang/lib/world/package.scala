package org.zhang.lib

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/19/11
 * Time: 3:41 AM
 */

package object world {
  implicit def location2vec2(l:Location) = l.loc
}