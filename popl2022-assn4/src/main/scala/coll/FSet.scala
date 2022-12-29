package coll

import collection.mutable.ListBuffer

trait FSet[+E] extends FIterable[E, FSet] {
  import HashTree.*

  private[coll] val tree: HashTree[E]

  def isEmpty: Boolean = tree eq Empty

  def add[U >: E](elem: U): FSet[U] = {
    val newTree = tree.add(elem)
    if (newTree != tree) then
      new FSet[U] {
        private[coll] val tree = newTree
      }
    else
      this
  }

  def foreach(action: E => Unit): Unit = {

    def foreachRec(tree: HashTree[E]): Unit = {
      tree match
        case Empty =>
        case Node(hash, left, right, elems) => {
          for(x <- elems) {
            action(x)
          }
          foreachRec(left)
          foreachRec(right)
        }
    }

    foreachRec(tree)
  }

  protected[this] def newBuilder[X]: Builder[X, FSet] =
    new Builder[X, FSet] {
      private val b = new ListBuffer[X];
      override def collect(elem: X) = b.addOne(elem);
      override def build: FSet[X] = {
        val set: FSet[X] = FSet()
        for (x <- b.reverse) {
          set.add(x)
        }
        set
      };
    }
}

object FSet {
  val EmptySet = new FSet[Nothing] {
    private[coll] val tree = HashTree.Empty
  }
}

