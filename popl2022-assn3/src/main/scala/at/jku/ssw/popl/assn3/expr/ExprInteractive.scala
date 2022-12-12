package at.jku.ssw.popl.assn3.expr

import at.jku.ssw.popl.assn3.field.Field
import at.jku.ssw.popl.assn3.expr.Expr.*

abstract class ExprInteractive[T] extends ExprParser[T] {

  def interact(bds: Map[String, T])(using field: Field[T]): Unit = {

    println("===================================================")
    println("Working with expressions")
    println("  Enter expressions or assignments")
    println("  Assignments, x = expr, extend variable bindings ")
    println("  Expressions will be evaluated and simplified ")
    println("  Terminate with '.'")
    println("===================================================")
    println

    var bdgs: Map[String, T] = bds
    println(s"Variable bindings: $bdgs")
    println("--------------------------------------------------")
    var terminate = false
    while (!terminate) {
      println
      print("Enter expression/assignment: ")
      val line = Console.in.readLine()
      if (line.startsWith(".")) {
        terminate = true
      } else {
        parseAssn(line) match
          case Success(bd@(v, e), _) => {
            try {
              val eVal = eval(e, bdgs)
              bdgs += (v, eVal)
              println(s"Variable bindings: $bdgs")
            } catch {
              case e: Exception => {
                println("Failed evaluation: " + e.getMessage)
              }
            }
          }
          case _ => parseExpr(line) match {
              case Success(expr, rest) => {
                println
                println(s"Expression: $expr")
                try {
                  val value = eval(expr, bdgs)
                  println(s"Value = $value")

                  val simplified = simplify(expr)
                  println(s"Simplified: $simplified")

                  val valueSimpl = eval(simplified, bdgs)
                  println(s"Value of simplified = $valueSimpl")
                } catch {
                  case e: Exception => {
                    println("Failed evaluation: " + e.getMessage)
                  }
                }
              }
              case Failure(excpt, rest) => {
                println(s"Failed parse with $excpt at $rest")
              }
              case _ => {
                println(s"Failed parse")
              }
            }
      }
      println("--------------------------------------------------")
    }
    
  }

}
