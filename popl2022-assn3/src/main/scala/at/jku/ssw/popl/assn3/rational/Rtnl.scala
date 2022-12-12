package at.jku.ssw.popl.assn3.rational

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

case class Rtnl(numer: Int, denom: Int) {
  override def toString: String = s"$numer/$denom"
}

object Rtnl {
  def apply(n: Int, d: Int) = {
    if (n == 0) then new Rtnl(0, 1)
    else {
      val g = gcd(n.abs, d.abs)
      if (n <= 0 && d < 0) then new Rtnl(-n / g, -d / g)
      else new Rtnl(n / g, d / g)
    }
  }
  def apply(n: Int) = new Rtnl(n, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // TODO: given for Field[Rtnl]
}

// Application

object RtnlApp extends App {

  object RtnlInteractive extends ExprInteractive[Rtnl] {
    def value: Parser[Rtnl] =
      wholeNumber ~ "/" ~ wholeNumber ^^ { case n ~ _ ~ d => Rtnl(n.toInt, d.toInt) } |
        wholeNumber ^^ { case n => Rtnl(n.toInt) }

  }
  RtnlInteractive.interact(Map("x" -> Rtnl(1, 2), "y" -> Rtnl(2, 3))) (using null) // TODO: delete using

}
