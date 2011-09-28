//package org.zhang.lib
//
//import collection.immutable.MapProxy
//import processing.core.PApplet._
//import java.io.{BufferedReader, Reader}
//
///**
//* Created by IntelliJ IDEA.
//* User: hellochar
//* Date: 6/19/11
//* Time: 4:09 AM
//*/
//
//private object ConfigFile {
//  def fromReader(r:BufferedReader) = {
//    r.readLine()
//  }
//}
//
//class ConfigFile(strings: Array[String]) extends Map[String, String] with MapProxy[String, String] {
//  this(r:BufferedReader) = this(fromReader(r))
//
//  private var self: Map[String, String] = _
//  private var selfCache: Map[String, Any] = Map()
//  reloadConfig();
//
//  private def reloadConfig() {
//    self = Map() ++ (loadStrings("config.txt") filterNot (s => s.startsWith("#") || s.equals("")) map (s => {
//      val k = s.split('=') map (_.trim); (k(0), k(1))
//    }))
//    configMapCache.clear()
//  }
//
//  private def findValue[A](m: (String) => A): ((String) => A) = (str: String) => configMapCache.getOrElseUpdate(str, m(configMap(str))).asInstanceOf[A]
//
//  val getInt = findValue(_.toInt)
//  val getFloat = findValue(_.toFloat)
//  val getBoolean = findValue(_.toBoolean)
//}