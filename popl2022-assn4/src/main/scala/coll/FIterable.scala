package coll

trait Builder[A, +C[+A]] {
  def collect(elem: A) : Unit
  def build : C[A]
}

trait FIterable[+E, +C[+A]] {

}