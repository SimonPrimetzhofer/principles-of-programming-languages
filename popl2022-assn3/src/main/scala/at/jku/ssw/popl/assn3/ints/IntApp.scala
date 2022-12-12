package at.jku.ssw.popl.assn3.ints

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

// Application

object IntApp extends App {

  object IntInteractive extends ExprInteractive[Int] {
    
    def value: Parser[Int] =
      wholeNumber ^^ { case digits => digits.toInt}

  }
  
  IntInteractive.interact(Map("x" -> 1, "y" -> 2))(using null) // TODO: delete using

}
