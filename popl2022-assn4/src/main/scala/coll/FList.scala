package coll

import scala.collection.mutable.ListBuffer

sealed trait FList[+E] extends FIterable[E, FList] {

}