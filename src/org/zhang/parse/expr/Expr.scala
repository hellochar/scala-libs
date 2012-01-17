package org.zhang.parse.expr

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/9/11
 * Time: 3:01 PM
 */
trait Expr { def eval:Float; }
case class Const(eval:Float) extends Expr

case class Unary(op:Expr, func:Float => Float) extends Expr {
  def eval = func(op.eval)
}

case class Neg(e:Expr) extends Unary(e, -_)

case class Binary(leftOp:Expr, rightOp:Expr, func:(Float, Float) => Float) extends Expr { //a restructuring is needed
  def eval = func(leftOp.eval, rightOp.eval)
}

case class Add(left:Expr, right:Expr) extends Binary(left, right, _ + _)
case class Sub(left:Expr, right:Expr) extends Binary(left, right, _ - _)
case class Mul(left:Expr, right:Expr) extends Binary(left, right, _ * _)
case class Div(left:Expr, right:Expr) extends Binary(left, right, _ / _)
case class Pow(left:Expr, right:Expr) extends Binary(left, right, math.pow(_, _).toFloat)