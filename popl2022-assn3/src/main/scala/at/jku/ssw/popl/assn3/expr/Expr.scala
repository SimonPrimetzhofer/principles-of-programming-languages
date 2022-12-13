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
      case Var(n) => bds.get(n).get
      case Add(left, right) => field.plus(eval(left, bds), eval(right, bds))
      case Minus(sub) => field.neg(eval(sub, bds))
      case Mult(left, right) => field.times(eval(left, bds), eval(right, bds))
      case Recip(sub) => field.recip(eval(sub, bds))
    }
  }

  def simplify[T](expr: Expr[T])(using field: Field[T]) : Expr[T] =
    expr match {
      case Lit(x) => Lit(x)
      case Var(n) => Var(n)
      case Add(left, right) => {
        val simpleLeft = simplify(left);
        val simpleRight = simplify(right);

        (simpleLeft, simpleRight) match {
          // a + 0 = a
          case (_, Lit(field.zero)) => simpleLeft
          case (Lit(field.zero), _) => simpleRight

          case (Minus(left), Var(right)) => {
            left match {
              case Var(v) => {
                if (v.equals(right)) {
                  Lit(field.zero)
                } else {
                  Add(simpleLeft, simpleRight)
                }
              }
            }
          }
          case (Var(left), Minus(right)) => {
            right match {
              case Var(v) => {
                if (v.equals(left)) {
                  Lit(field.zero)
                } else {
                  Add(simpleLeft, simpleRight)
                }
              }
            }
          }
          // a + -a = 0
          case (Lit(x), Lit(y)) => {
            if (x.equals(field.neg(y)) || y.equals(field.neg(x))) {
              Lit(field.zero)
            } else {
              Add(simpleLeft, simpleRight)
            }
          }
          case _ => Add(simpleLeft, simpleRight)
        }
      }
      case Minus(sub) => {
        val simpleSub = simplify(sub);

        simpleSub match {
          case Minus(subsub) => subsub
          case _ => Minus(simpleSub)
        }
      }
      case Mult(left, right) => {
        val simpleLeft = simplify(left);
        val simpleRight = simplify(right);

        (simpleLeft, simpleRight) match {
          // a * 0 = 0
          case (_, field.zero) | (field.zero, _) => Lit(field.zero)

          // a * 1 = a
          case (_, Lit(field.one)) => simpleLeft
          case (Lit(field.one), _) => simpleRight

          // a * a^-1 = 1
          case (Lit(x), Lit(y)) => {
            if (x.equals(field.neg(y)) || y.equals(field.neg(x))) {
              Lit(field.one)
            } else {
              Mult(simpleLeft, simpleRight)
            }
          }
          case (_, _) => Mult(simpleLeft, simpleRight)
        }
      }
      case Recip(sub) => {
        val simpleSub = simplify(sub);

        simpleSub match {
          case Recip(subsub) => subsub
          case _ => Recip(simpleSub)
        }
      }
    }
}