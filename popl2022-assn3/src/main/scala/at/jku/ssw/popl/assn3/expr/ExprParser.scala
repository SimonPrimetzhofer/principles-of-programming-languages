package at.jku.ssw.popl.assn3.expr

import at.jku.ssw.popl.assn3.field.Field
import scala.util.parsing.combinator.JavaTokenParsers

abstract class ExprParser[T] extends JavaTokenParsers {
  
  def value: Parser[T]

  def parseExpr(input: String): ParseResult[Expr[T]] = parse(expr, input)

  def expr: Parser[Expr[T]] = literal | variable | add | minus | mult | recip

  def variable: Parser[Var[T]] = ident ^^ { n => Var(n) }

  def literal: Parser[Lit[T]] = value ^^ { v => Lit(v) }

  def add: Parser[Add[T]] = "(" ~> expr ~ "+" ~ expr <~ ")" ^^ { case l ~ op ~ r => Add(l, r) }

  def minus: Parser[Minus[T]] = "(" ~> "-" ~> expr <~ ")" ^^ { case e => Minus(e) }

  def mult: Parser[Mult[T]] = "(" ~> expr ~ "*" ~ expr <~ ")" ^^ { case l ~ op ~ r => Mult(l, r) }

  def recip: Parser[Recip[T]] = "(" ~> "/" ~> expr <~ ")" ^^ { case e => Recip(e) }

  def parseAssn(input: String): ParseResult[(String, Expr[T])] = parse(assignment, input)

  def assignment: Parser[(String, Expr[T])] = ident ~ "=" ~ expr ^^ { case n ~ _ ~ v => (n, v) }

}

