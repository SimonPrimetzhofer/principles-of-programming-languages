package coll

import scala.collection.mutable.ListBuffer

sealed trait FList[+E] extends FIterable[E, FList]{
  def isEmpty: Boolean = this eq FNil

  def add[U >: E](elem: U): FList[U] = new FCons[U](elem, this)

  def foreach(action: E => Unit): Unit = {
    this match {
      case FNil =>
      case FCons(head, tail) => {
        action(head)
        tail.foreach(action)
      }
    }
  }

  protected[this] def newBuilder[X]: Builder[X, FList] =
    new Builder[X, FList] {
      private val b = new ListBuffer[X];
      override def collect(elem: X) = b.addOne(elem);
      override def build: FList[X] = {
        val list: FList[X] = FNil
        for (x <- b.reverse) {
          list.add(x)
        }
        list
      };
    }
}

case object FNil extends FList[Nothing]
case class FCons[+E](head: E, tail: FList[E]) extends FList[E]
