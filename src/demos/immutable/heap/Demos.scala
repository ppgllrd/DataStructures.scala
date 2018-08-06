/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.immutable.heap

import dataStructures.immutable.heap.WBLH

object Demos extends App {
  val factory = WBLH.factory[Int]
  var h1 = factory.empty

  h1 = h1.insert(5)
  h1 = h1.insert(7)
  h1 = h1.insert(3)
  h1 = h1.insert(9)
  h1 = h1.insert(1)

  println(h1)

  println(h1.minElem)

  h1 = h1.delMin

  println(h1)


  val h2 = factory.empty.insert(7).insert(13)

  val h3 = h1.merge(h2)

  println(h3)

  var xs = Array(10, 3, 2, 10, 1, 9, 7)
  WBLH.mergeableOps[Int].heapSort(xs)
  println(xs.mkString(" "))
}
