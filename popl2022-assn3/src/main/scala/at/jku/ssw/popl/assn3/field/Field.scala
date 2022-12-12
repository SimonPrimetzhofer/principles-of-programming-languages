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
  given intField: Field[Int] = new Field[Int]:
    def plus(x: Int, y: Int): Int = x + y;
    def times(x: Int, y: Int): Int = x * y;
    def neg(x: Int): Int = -x;
    def recip(x: Int): Int = 1/x;
    val zero: Int = 0;
    val one: Int = 1;
  
  given doubleField: Field[Double] = new Field[Double]:
    def plus(x: Double, y: Double): Double = x + y;
    def times(x: Double, y: Double): Double = x * y;
    def neg(x: Double): Double = -x;
    def recip(x: Double): Double = 1.0/x;
    val zero: Double = 0.0;
    val one: Double = 1.0;
}