package coll

private[coll] enum HashTree[+E] {
  case Empty extends HashTree[Nothing]
  case Node(hash: Int, left: HashTree[E], right: HashTree[E], values: List[E]) extends HashTree[E]

  def add[U >: E](value: U): HashTree[U] = {

    def addRec(tree: HashTree[U], hash: Int): HashTree[U] = {
      tree match {
        case Empty => {
          Node(hash, Empty, Empty, List(value))
        }
        case node@Node(h, l, r, vs) => {
          if (hash == h) then {
            if (vs.contains(value)) node
            else Node(h, l, r, value :: vs)
          }
          else if (hash < h) then {
            val newLeft = addRec(l, hash)
            if (newLeft eq l) then node
            else Node(h, newLeft, r, vs)
          } else {
            val newRight = addRec(r, hash)
            if (newRight eq r) then node
            else Node(h, l, newRight, vs)
          }
        }
      }
    }

    addRec(this, value.hashCode())
  }
}

object HashTree {
  def printTree[E](tree: HashTree[E]): Unit = {

    def printTreeRec(tree: HashTree[E], indent: String): Unit = {
      tree match
        case HashTree.Empty => //println(s"$indent -")
        case HashTree.Node(hash, left, right, elems) => {
          println(s"$indent $hash - ${elems.toString()}")
          printTreeRec(left, indent + "  ")
          printTreeRec(right, indent + "  ")
        }
    }

    printTreeRec(tree, "")
  }
}

