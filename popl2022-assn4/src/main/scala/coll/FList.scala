package coll

import scala.collection.mutable.ListBuffer

sealed trait FList[+E] extends FIterable[E, FList] {
  case object FNil extends FList[Nothing];
  case class FCons[+E](head: E, tail: List[E]) extends FList[E];

  override def isEmpty: Boolean = this eq Nil

  override def add[U >: E](elem: U): FList[U] = {
    val b = newBuilder[U];
    foreach(elem => b.collect(elem));
    b.build;
  }

  override def foreach(action: E => Unit): Unit = {
    var current = this
    while (!current.isEmpty) {
      current match {
        case FCons(head, tail) => {
          action(head);
          current = tail;
        }
      }
    }
  }

  override protected[this] def newBuilder[X]: Builder[X, FList] =
    new Builder[X, FList] {
      private val b = new ListBuffer[X];

      override def collect(elem: X) = b.addOne(elem);
      override def build: FList[X] = new List[X](b.head, b.tail);
    }
}