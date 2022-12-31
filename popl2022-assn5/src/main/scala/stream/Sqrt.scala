package stream

object Sqrt {

  def sqrtStream(x: Double): LStream[Double] = LStream.iterate(x / 2, ai => (ai + x / ai) / 2)

  def approximate[T](pred: T => Boolean, sequ: LStream[T]): Option[T] = sequ.find(pred)

  def approximateSqrt(x: Double, epsilon: Double): Option[Double] =
    approximate(a => Math.abs((a * a) - x) < epsilon, sqrtStream(x))

  def main(args: Array[String]): Unit = {

    val sqrt9 = approximateSqrt(9.0, 0.000000001)
    println(s"Square root of 9 = $sqrt9")

    val sqrt16 = approximateSqrt(16.0, 0.000000001)
    println(s"Square root of 16 = $sqrt16")

    val sqrt25 = approximateSqrt(25.0, 0.000000001)
    println(s"Square root of 25 = $sqrt25")

  }
}
