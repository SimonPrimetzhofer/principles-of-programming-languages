package at.jku.ssw.popl.assn3.field

trait Field[F] {
  def plus(x: F, y: F): F
  def times(x: F, y: F): F
  def neg(x: F): F
  def recip(x: F): F
  val zero: F
  val one: F
}

object Field {
  // TODO: given for Field[Int]
  // TODO: given for Field[Double]
}