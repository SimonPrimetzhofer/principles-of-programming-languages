package coll2

import scala.collection.mutable.ListBuffer

import coll.{FIterable, Builder}

sealed trait LinkedList[+A] extends FIterable[A, LinkedList] {
  def isEmpty: Boolean

  val size: Int

  def add[B >: A](elem: B) = new Node[B](elem, this)

  def foreach(action: A => Unit): Unit =
    this match
      case Empty =>
      case Node(elem, tail)  => {
        action(elem)
        tail.foreach(action)
      }

  protected[this] def newBuilder[X]: Builder[X, LinkedList] =
    new Builder[X, LinkedList] {
      val b : ListBuffer[X] = new ListBuffer[X]
      def collect(elem: X): Unit = b += elem
      def build: LinkedList[X] = {
        var l : LinkedList[X] = Empty
        for (x <- b.reverse) {
          l = l.add(x)
        }
        l
      }
    }

}

case object Empty extends LinkedList[Nothing] {
  val isEmpty: Boolean = true
  val size = 0
}

case class Node[+A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
  val isEmpty: Boolean = false
  val size = tail.size + 1
}

object LinkedList {
  def apply[A](elems : A*) : LinkedList[A] = {
    var l : LinkedList[A] = Empty
    for (e <- elems.reverse) {
      l = Node[A](e, l)
    }
    l
  }
}

object LinkedListApp extends App {
    var ll : LinkedList[Int] = Empty
    ll = ll.add(1)
    ll = ll.add(2)

    ll.map(x => x * x).foreach(println(_))
}