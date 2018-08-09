/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait MergeableHeapFactory[A] extends HeapFactory[A] {
  override type Heap <: dataStructures.immutable.heap.MergeableHeap[A]
    with dataStructures.immutable.heap.MergeableHeapLike[Heap, A]

  private def mergePairs(xs: List[Heap]): List[Heap] = xs match {
    case List() | List(_) =>
      xs
    case h1 :: h2 :: hs =>
      h1.merge(h2) :: mergePairs(hs)
  }

  def makeHeap(xs: A*): Heap = {
    require(xs.nonEmpty, "makeHeap needs a non-empty sequence")

    var hs = List[Heap]()
    for (x <- xs)
      hs ::= singleton(x)

    while (hs.length > 1)
      hs = mergePairs(hs)

    hs.head
  }

  def heapSort(xs: Array[A]): Unit = {
    var h = makeHeap(xs: _*)
    for (i <- 0 until xs.length) {
      xs(i) = h.minElem
      h = h.delMin
    }
  }
}