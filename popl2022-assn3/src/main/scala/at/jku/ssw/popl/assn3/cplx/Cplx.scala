package at.jku.ssw.popl.assn3.cplx

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

case class Cplx(real: Double, imag: Double) {
  override def toString: String = s"(${real}, ${imag})"
}

object Cplx {
  def apply(real: Double, imag: Double) =  new Cplx(real, imag)
  def apply(real: Double) = new Cplx(real, 0.0)

  given cplxField: Field[Cplx] = new Field[Cplx] {
    def plus(x: Cplx, y: Cplx): Cplx = new Cplx(x.real + y.real, x.imag + y.imag);

    def times(x: Cplx, y: Cplx): Cplx = new Cplx(
      (x.real * y.real) - (x.imag * y.imag),
      (x.real * y.imag) + (x.imag * y.real));

    def neg(x: Cplx): Cplx = new Cplx(-x.real, -x.imag);

    def recip(x: Cplx): Cplx = new Cplx(
      x.real / ((x.real * x.real) + (x.imag * x.imag)),
      -x.imag / ((x.real * x.real) + (x.imag * x.imag)));

    val zero: Cplx = new Cplx(0.0, 0.0);
    val one: Cplx = new Cplx(1.0, 0.0);
  }
}

// Application

object CplxApp extends App {

  object CplxInteractive extends ExprInteractive[Cplx] {
    def value: Parser[Cplx] =
      "(" ~> wholeNumber ~ "," ~ wholeNumber <~ ")" ^^ { case r ~ _ ~ i => Cplx(r.toInt, i.toInt) } |
        "(" ~> wholeNumber <~ ")" ^^ { case r => Cplx(r.toInt) }

  }
  CplxInteractive.interact(Map("x" -> Cplx(1, 1), "y" -> Cplx(2, 2)))

}
