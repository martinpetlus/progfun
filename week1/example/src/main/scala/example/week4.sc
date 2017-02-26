//abstract class Boolean {
//  def ifThenElse[T](t: => T, e: => T): T
//  def && (x: => Boolean): Boolean = ifThenElse(x, false)
//  def || (x: => Boolean): Boolean = ifThenElse(true, x)
//  def unary_!: Boolean = ifThenElse(false, true)
//  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
//  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
//  def < (x: Boolean): ifThenElse(false, x)
//}
//
//object true extends Boolean {
//  def ifThenElse[T](t: => T, e: => T) = t
//}
//
//object false extends Boolean {
//  def ifThenElse[T](t: => T, e: => T) = e
//}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new IllegalArgumentException("predecessor of zero")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat =
    if (that.isZero) this
    else throw new IllegalArgumentException("argument is non-zero")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = // new Succ(n + that)
    if (that.isZero) this
    else successor + that.predecessor
  def - (that: Nat): Nat =
    if (that.isZero) this
    else predecessor - that.predecessor
}

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Var(v: String) extends Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Var(v) => v
  case Prod(Sum(e1, e2), e3) => "(" + show(Sum(e1, e2)) + ") * " + show(e3)
  case Prod(e1, Sum(e2, e3)) => show(e1) + " * (" + show(Sum(e2, e3)) + ")"
  case Prod(e1, e2) => show(e1) + " * " + show(e2)
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
}

show(Sum(Number(1), Number(44)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (y < x) y :: insert(x, ys) else x :: xs
}

insert(7, List(2, 3, 9, 10))
