package at.jku.ssw.popl.assn3.doubles

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

// Application

object DoubleApp extends App {

  object DoubleInteractive extends ExprInteractive[Double] {
    def value: Parser[Double] =
      floatingPointNumber ^^ { case str => str.toDouble}

  }
  
  DoubleInteractive.interact(Map("x" -> 1.0, "y" -> 2.0))(using null) // TODO: delete using
}
