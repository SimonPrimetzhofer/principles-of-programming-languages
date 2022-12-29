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

  def map[R](f: E => R) : C[R] = {
    val b = newBuilder[R]
    foreach(elem => b.collect(f(elem)))
    b.build
  }

  def filter(p: E => Boolean) : C[E] = {
    val b = newBuilder[E]
    foreach(elem => if (p(elem)) then b.collect(elem))
    b.build

  }

  /*def fold[R](z: R)(acc: (R, E) => R): R = {
    var curr = z
    foreach(elem => curr = acc(elem, curr))
    curr
  }*/

  /*def sum(f: E => Int) : Int = {
    var sum = 0
    fold(elem => )
  }

  def count : Int – counting the elements in this iterable

  def toString(sep: String = ", ", pre: String = "", post: String*/
}