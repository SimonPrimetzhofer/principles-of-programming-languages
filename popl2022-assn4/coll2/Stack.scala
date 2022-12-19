package coll2

import coll.{Builder, FIterable}

import scala.collection.mutable.ListBuffer

abstract class Stack[+A] extends FIterable[A, Stack] {
  thisStack =>

  import Stack.*

  def isEmpty: Boolean
  val size: Int
  val first : Nd[A]

  override def foreach(action: A => Unit): Unit = {
    var n = first
    while (n != null) {
      action(n.value)
      n = n.next
    }
  }

  override def newBuilder[X]: Builder[X, Stack] =
    new Builder[X, Stack] {
      val b: ListBuffer[X] = new ListBuffer[X]
      def collect(elem: X): Unit = b += elem
      def build: Stack[X] =
        Stack(b.toList)
    }

  def top : A = if (first != null) first.value
                else throw new NoSuchElementException("top of empty Stack")

  def pop : Stack[A] =
    if (first != null) then new Stack[A] {
      val size: Int = thisStack.size - 1
      val first: Nd[A] = thisStack.first.next
      def isEmpty: Boolean = first == null
    } else throw new NoSuchElementException("top of empty Stack")

  def push[B >: A](elem: B) = new Stack[B] {
    val size: Int = thisStack.size + 1
    val first: Nd[B] = Nd[B](elem, thisStack.first)
    def isEmpty: Boolean = false
  }

  def add[B >: A](elem: B) = push(elem)

}

object Stack {

  object Empty extends Stack[Nothing] {
    val size: Int = 0
    val first: Nd[Nothing] = null
    def isEmpty: Boolean = true
  }

  def apply[E](elems: Iterable[E]): Stack[E] = {
    var stack: Stack[E] = Empty
    for (e <- elems.toList.reverse) {
      stack = stack.push(e)
    }
    stack
  }

  def apply[E](elems: E*): Stack[E] = {
    var stack : Stack[E] = Empty
    for (e <- elems.reverse) {
      stack = stack.push(e)
    }
    stack
  }

}

private[coll2] case class Nd[+E](value: E, next: Nd[E])

object StackApp extends App {
    var ll : Stack[Int] = Stack.Empty
    ll = ll.push(1)
    ll = ll.push(2)

    ll.map(i => i * 2).foreach(println(_))

  println(ll.fold(0)((r, e) => r + e))
  println(ll.count)
  println(ll.toString(pre = "[", post = "]"))
}