package stream

abstract sealed trait LStream[+E] {
  def head: E = {
    this match {
      case LCons(hdFn, tlFn) => hdFn()
      //case Nomore => head // TODO
    }
  }
  def tail: LStream[E] = {
    this match {
      case Nomore => Nomore
      case LCons(hdFn, tlFn) => tlFn()
    }
  }
  def map[R](fn: E => R): LStream[R] = {
    this match {
      case Nomore => Nomore
      case LCons(hdFn, tlFn) => LCons(() => fn(hdFn()), () => tlFn().map(fn))
    }
  }
  def filter(pred: E => Boolean): LStream[E] = {
    this match {
      case Nomore => Nomore
      case LCons(hdFn, tlFn) =>
        if (pred(hdFn())) then
          LCons(hdFn, () => tlFn().filter(pred))
        else
          LCons(() => tlFn().head, () => tlFn().tail.filter(pred))
    }
  }
  def take(n: Int): LStream[E] = {
    this match {
      case Nomore => Nomore
      case LCons(hdFn, tlFn) => if (n > 0) then LCons(hdFn, () => tlFn().take(n - 1)) else Nomore
    }
  }
  def foreach(action: E => Unit): Unit = ???
  def toList : List[E] = ???
  def find(pred: E => Boolean): Option[E] = ???
}

case object Nomore extends LStream[Nothing];
case class LCons[+E](hdFn: () => E, tlFn: () => LStream[E]) extends LStream[E];

object LStream {
  def apply[E](head: => E, tail: => LStream[E]) : LStream[E] = {
    new LStream[E] {
      override def head = head
      override def tail = tail
    }
  }
  def iterate[E](seed: E, next: E => E) : LStream[E] = ???
  def numsFrom(n: Int): LStream[Int] = ???
}

def isPrime(n: Int): Boolean =
  (n > 1) && !(2 to scala.math.sqrt(n).toInt).exists(x => n % x == 0)


object Main {
  def main(args: Array[String]): Unit = {

  }
}