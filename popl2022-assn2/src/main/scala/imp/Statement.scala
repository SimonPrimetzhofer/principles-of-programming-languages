package imp

import imp.Expr._

abstract class Statement
case object Skip extends Statement
case object Empty extends Statement
case class Error(msg: String, stmt: Statement) extends Statement
case class PrintStmt(text: String, expr: Expr) extends Statement
case class AssnStmt(lExpr: Var, rExpr: Expr) extends Statement
case class IfStmt(cond: Expr, thenComm: Statement, elseComm: Statement) extends Statement
case class WhileStmt(cond: Expr, body: Statement) extends Statement
case class BlockStmt(sList: List[Statement]) extends Statement {
  def replaceFirst(s: Statement): Statement = {
    s match {
      case Empty => if (sList.isEmpty) Empty else BlockStmt(sList.tail)
      case Skip => if (sList.isEmpty) Empty else BlockStmt(sList.tail)
      case Error(msg, es) => Error(msg, BlockStmt(es :: sList.tail))
      case _ => BlockStmt(s :: sList.tail)
    }
  }
}

object Statement {

  def exec(stmt: Statement): (Statement, State) = exec(stmt, new State())

  def exec(stmt: Statement, state: State): (Statement, State) = {
    stmt match {
      case Skip => (Empty, state)
      case Empty => (Empty, state)
      case Error(msg, es) => (Error(msg, es), state)
      case PrintStmt(text, expr) => {
        val evalExpr = eval(expr, state)
        evalExpr match {
          case Some(evalExpr) => {
            Console.println(text + " " + evalExpr.x)
            (Empty, state)
          }
          case _ => (Error("Cannot evaluate expression of print statement", Empty), state)
        }
      }
      case AssnStmt(left, right) => {
        val leftExpr = eval(left, state)
        val rightExpr = eval(right, state)
        (leftExpr, rightExpr) match {
          case (_, Some(rightExpr)) => (Empty, state.updt(left.name, rightExpr))
          case _ => (Error("Cannot evaluate assignment", Empty), state)
        }
      }
      case IfStmt(cond, thenComm, elseComm) => {
        val condEval = eval(cond, state)
        condEval match {
          case Some(BoolVal(condEval)) => {
            if (condEval) {
              exec(thenComm, state)
            } else {
              exec(elseComm, state)
            }
          }
          case _ => (Error("Found invalid if statement", Empty), state)
        }
      }
      case WhileStmt(cond, body) => {
        val condEval = eval(cond, state)
        condEval match {
          case Some(BoolVal(condEval)) => {
            if (condEval) {
              val (nextStatement, nextState) = exec(body, state)
              (WhileStmt(cond, body), nextState)
            } else {
              (Empty, state)
            }
          }
          case _ => (Error("Found invalid while statement", Empty), state)
        }
      }
      case BlockStmt(sList) => {
        var blockStmt = BlockStmt(sList)
        var currState = state

        while (blockStmt.sList.nonEmpty) {
          val (headStmt, headState) = exec(blockStmt.sList.head, currState)
          currState = headState
          val nextStmt = blockStmt.replaceFirst(headStmt)
          nextStmt match {
            case BlockStmt(sList) => blockStmt = BlockStmt(sList)
            case _ => (headStmt, headState)
          }
        }
        (Empty, currState)
      }
      case _ => (Error("Found invalid block statement", Empty), state)
    }
  }
}

