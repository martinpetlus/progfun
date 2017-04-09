package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def check(names: Set[String], expr: Expr, refs: Map[String, Signal[Expr]]): Boolean = expr match {
    case Literal(v) => true
    case Ref(name) =>
      if (names(name)) false
      else check(names + name, getReferenceExpr(name, refs), refs)
    case Plus(a, b) => check(names, a, refs) && check(names, b, refs)
    case Minus(a, b) => check(names, a, refs) && check(names, b, refs)
    case Times(a, b) => check(names, a, refs) && check(names, b, refs)
    case Divide(a, b) => check(names, a, refs) && check(names, b, refs)
  }

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, exprSignal) => name -> Signal {
        if (check(Set(name), exprSignal(), namedExpressions))
          eval(exprSignal(), namedExpressions)
        else
          Double.NaN
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
