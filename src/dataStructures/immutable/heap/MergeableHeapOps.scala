/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait MergeableHeapOps[A, This <: MergeableHeap[A, This]] {
  def singleton(elem: A): This

  def mergePairs(xs: List[This]): List[This] = xs match {
    case List() | List(_) =>
      xs
    case h1 :: h2 :: hs =>
      h1.merge(h2) :: mergePairs(hs)
  }

  def makeHeap(xs: Seq[A]): This = {
    require(xs.nonEmpty, "makeHeap needs a non-empty sequence")

    var hs = List[This]()
    for (x <- xs)
      hs ::= singleton(x)

    while (hs.length > 1)
      hs = mergePairs(hs)

    hs.head
  }

  def heapSort(xs: Array[A]): Unit = {
    var h = makeHeap(xs)
    for (i <- 0 until xs.length) {
      xs(i) = h.minElem
      h = h.delMin.asInstanceOf[This] // this could be fixed by making delMin return an object of same type as this
    }
  }
}
