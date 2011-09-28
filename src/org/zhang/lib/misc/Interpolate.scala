package org.zhang.lib.misc

object Interpolate {
//  def linear[T1 <: Numeric, T2 <: Numeric, T3 <: Numeric, T4 <: Numeric, T5 <: Numeric](x:T1, startD:T2, endD:T3, startR:T4, endR:T5) =
//    ((x - startD) / (endD - startD)) * (endR - startR) + startR;

  def linear(x:Float, startD:Float, endD:Float, startR:Float, endR:Float) =
    ((x - startD) / (endD - startD)) * (endR - startR) + startR;

//  private def map(x:Float, )

//  def square(x:Float, startD:Float, endD:Float, startR:Float, endR:Float) =

  //I wanna be able to write something like this: Interpolate.linear(x, 0, 10, 4, 9)
  //Interpolate.square(x,
}