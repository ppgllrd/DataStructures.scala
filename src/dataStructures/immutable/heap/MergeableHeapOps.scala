package dataStructures.immutable.heap

trait MergeableHeapOps[This[T] <: MergeableHeap[T, This]] {
  def singleton[A](elem: A): This[A]

  def mergePairs[A](xs: List[This[A]])(implicit ord: Ordering[A]): List[This[A]] = xs match {
    case List() | List(_) =>
      xs
    case h1 :: h2 :: hs =>
      h1.merge(h2) :: mergePairs(hs)
  }

  def makeHeap[A](xs: Seq[A])(implicit ord: Ordering[A]): This[A] = {
    require(xs.nonEmpty, "makeHeap needs a non-empty sequence")

    var hs = List[This[A]]()
    for (x <- xs)
      hs ::= singleton(x)

    while (hs.length > 1)
      hs = mergePairs(hs)

    hs.head
  }

  def heapSort[A](xs: Array[A])(implicit ord: Ordering[A]): Unit = {
    var h = makeHeap(xs)
    for (i <- 0 until xs.length) {
      xs(i) = h.minElem
      h = h.delMin.asInstanceOf[This[A]] // this could be fixed by making delMin return an object of same type as this
    }
  }
}
