package at.jku.ssw.popl.assn3.cplx

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

case class Cplx(real: Double, imag: Double) {
  override def toString: String = s"(${real}, ${imag})"
}

object Cplx {
  def apply(real: Double, imag: Double) =  new Cplx(real, imag)
  def apply(real: Double) = new Cplx(real, 0.0)

  // TODO: given for Field[Cplx]
}

// Application

object CplxApp extends App {

  object CplxInteractive extends ExprInteractive[Cplx] {
    def value: Parser[Cplx] =
      "(" ~> wholeNumber ~ "," ~ wholeNumber <~ ")" ^^ { case r ~ _ ~ i => Cplx(r.toInt, i.toInt) } |
        "(" ~> wholeNumber <~ ")" ^^ { case r => Cplx(r.toInt) }

  }
  CplxInteractive.interact(Map("x" -> Cplx(1, 1), "y" -> Cplx(2, 2))) (using null) // TODO: delete using

}
