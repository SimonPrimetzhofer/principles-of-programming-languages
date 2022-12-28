package coll

import collection.mutable.ListBuffer

trait FSet[+E] extends FIterable[E, FSet] {
  import HashTree.*

  val tree: HashTree[E]

  override def isEmpty: Boolean = ???

  override def add[U >: E](elem: U): FSet[U] = ???

  override def foreach(action: E => Unit): Unit = ???

  override protected[this] def newBuilder[X]: Builder[X, FSet] = ???
}