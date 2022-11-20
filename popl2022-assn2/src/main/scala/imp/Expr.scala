package imp

import scala.io.StdIn

abstract class Expr
abstract class Val(val x: AnyVal) extends Expr
case class IntVal(i: Int) extends Val(i)
case class BoolVal(b: Boolean) extends Val(b)
case class Var(name: String) extends Expr
case class UnyExpr(op: String, expr: Expr) extends Expr
case class BinExpr(op: String, left: Expr, right: Expr) extends Expr
case class ReadInt(prompt: String) extends Expr
case class ReadBool(prompt: String) extends Expr

object Expr {

  private val intOp = List("+", "-", "*", "/", "%")
  private val relOp = List("<", ">", ">=", "<=")
  private val eqOp = List("==", "!=")
  private val boolOp = List("&&", "||")

  def eval(expr: Expr, state: State): Option[Val] = {
      expr match {
        case value: Val => Some(value)
        case Var(value) => state.read(value)
        case ReadInt(prompt) => evalReadInt(prompt)
        case ReadBool(prompt) => evalReadBool(prompt)
        case UnyExpr(op, expr) => evalUnyExpr(op, expr, state)
        case BinExpr(op, left, right) => evalBinExpr(op, left, right, state)
      }
  }

  private def evalReadBool(prompt: String): Option[BoolVal] = {
    print(prompt + " (true | false) => ")
    try {
      Some(BoolVal(scala.io.StdIn.readBoolean()))
    } catch {
      case e: Exception => None
    }
  }

  private def evalReadInt(prompt: String): Option[IntVal] = {
    print(prompt + " => ")
    try {
      Some(IntVal(scala.io.StdIn.readInt()))
    } catch {
      case e: Exception => None
    }
  }

  // for - and ! operator
  private def evalUnyExpr(op: String, expr: Expr, state: State): Option[Val] = {
    val evalExpr = eval(expr, state)
    op match {
      case "-" => {
        evalExpr match {
          case Some(IntVal(evalExpr)) => Some(IntVal(-evalExpr))
          case _ => None
        }
      }
      case "!" => {
        evalExpr match {
          case Some(BoolVal(evalExpr)) => Some(BoolVal(!evalExpr))
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def evalBinExpr(op: String, left: Expr, right: Expr, state: State): Option[Val] = {
    val evalLeft = eval(left, state)
    val evalRight = eval(right, state)

    (evalLeft, evalRight) match {
      case (Some(IntVal(evalLeft)), Some(IntVal(evalRight))) => {
        op match {
          case "+" => Some(IntVal(evalLeft + evalRight))
          case "-" => Some(IntVal(evalLeft - evalRight))
          case "*" => Some(IntVal(evalLeft * evalRight))
          case "/" => Some(IntVal(evalLeft / evalRight))
          case "%" => Some(IntVal(evalLeft % evalRight))
          case "<" => Some(BoolVal(evalLeft < evalRight))
          case ">" => Some(BoolVal(evalLeft > evalRight))
          case "<=" => Some(BoolVal(evalLeft <= evalRight))
          case ">=" => Some(BoolVal(evalLeft >= evalRight))
          case "==" => Some(BoolVal(evalLeft == evalRight))
          case "!=" => Some(BoolVal(evalLeft != evalRight))
          case _ => None
        }
      }
      case (Some(BoolVal(evalLeft)), Some(BoolVal(evalRight))) => {
        op match {
          case "&&" => Some(BoolVal(evalLeft && evalRight))
          case "||" => Some(BoolVal(evalLeft || evalRight))
          case "==" => Some(BoolVal(evalLeft == evalRight))
          case "!=" => Some(BoolVal(evalLeft != evalRight))
          case _ => None
        }
      }
      case _ => None
    }
  }
}