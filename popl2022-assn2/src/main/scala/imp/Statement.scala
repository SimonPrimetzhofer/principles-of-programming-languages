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

  def exec(stmt: Statement, state: State): (Statement, State) = ???
    // TODO
}

