package at.jku.ssw.popl.assn3.expr

import at.jku.ssw.popl.assn3.field.Field

sealed abstract class Expr[T] {
  override def toString: String = Expr.show(this)
}
case class Lit[T](value: T) extends Expr[T]
case class Var[T](name: String) extends Expr[T]
abstract class UnyExpr[T](sub: Expr[T]) extends Expr[T]
case class Minus[T](sub: Expr[T]) extends UnyExpr[T](sub)
case class Recip[T](sub: Expr[T]) extends UnyExpr[T](sub)
abstract class BinExpr[T](left: Expr[T], right: Expr[T]) extends Expr[T]
case class Add[T](left: Expr[T], right: Expr[T]) extends BinExpr[T](left, right)
case class Mult[T](left: Expr[T], right: Expr[T]) extends BinExpr[T](left, right)

object Expr {

  def show[T](expr: Expr[T]): String = {
    expr match {
      case Lit(x) => x.toString
      case Var(n) => n
      case Add(left, right) => "(" + show(left) + " + " + show(right) + ")"
      case Minus(sub) => "(- " + show(sub) + ")"
      case Mult(left, right) => "(" + show(left) + " * " + show(right) + ")"
      case Recip(sub) => "(/ " + show(sub) + ")"
    }
  }

  def eval[T](expr: Expr[T], bds: Map[String, T])(using field: Field[T]) : T = {
    expr match {
      case Lit(x) => x
      case _ => ??? // TODO eval for expressions
    }
  }

  def simplify[T](expr: Expr[T])(using field: Field[T]) : Expr[T] = ??? // TODO

}