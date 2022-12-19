package coll

import collection.mutable.ListBuffer

trait FSet[+E] extends FIterable[E, FSet] {
  import HashTree.*

  val tree: HashTree[E]

}
