package coll

trait Builder[A, +C[+A]] {
  def collect(elem: A) : Unit
  def build : C[A]
}

trait FIterable[+E, +C[+A]] {
  def isEmpty: Boolean;

  def add[U >: E](elem: U): C[U];

  def foreach(action: E => Unit): Unit;

  protected[this] def newBuilder[X]: Builder[X, C];
}