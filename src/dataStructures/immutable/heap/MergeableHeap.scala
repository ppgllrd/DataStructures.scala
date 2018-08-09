/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait MergeableHeapLike[+This, A] extends HeapLike[This, A] {
  def merge[That >: This](that: That): That
}

trait MergeableHeap[A] extends Heap[A] with MergeableHeapLike[MergeableHeap[A], A]