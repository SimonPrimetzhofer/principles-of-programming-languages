package coll2

import coll.{Builder, FIterable}

import scala.collection.mutable.ListBuffer

abstract class SortedSet[+A](using ord: Ordering[A]) extends FIterable[A, SortedSet] {
  def isEmpty : Boolean

  def add[B >: A](elem: B)(using ord: Ordering[B]): SortedSet[B] = {
    this match {
      case EmptyTree => BinNode(elem, EmptyTree, EmptyTree)
      case BinNode(value, left, right) =>
        if ord.gt(elem, value) then BinNode(value, left, right.add(elem))
        else if ord.lt(elem, value) then BinNode(value, left.add(elem), right)
        else this
    }
  }

  def foreach(action: A => Unit): Unit = {
    this match {
      case EmptyTree =>
      case BinNode(elem, left, right) => {
        action(elem)
        left.foreach(action)
        right.foreach(action)
      }
    }
  }

  protected[this] def newBuilder[X]: Builder[X, SortedSet] =
    new Builder[X, SortedSet] {
      val b: ListBuffer[X] = new ListBuffer[X]

      def collect(elem: X): Unit = b += elem

      def build: SortedSet[X] = {
        var t: SortedSet[X] = EmptyTree
        for (x <- b.reverse) {
          //t = t.add(x)
        }
        t
      }
    }

}

case object EmptyTree extends SortedSet[Nothing] {
  val isEmpty: Boolean = true
}

case class BinNode[+A](elem: A, left: SortedSet[A], right: SortedSet[A])(using Ordering[A]) extends SortedSet[A] {
  val isEmpty: Boolean = false
}

object SortedSetApp extends App {

  /*var set : SortedSet[Int] = EmptyTree
  set = set.add(4)
  set = set.add(2)
  set = set.add(5)
  set = set.add(3)
  set = set.add(1)
  set = set.add(7)
  println(set)
*/
}
