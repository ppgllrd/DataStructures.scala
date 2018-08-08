/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait MergeableHeap[This <: MergeableHeap[This, A], A] extends Heap[This, A] {
  def merge(that: This): This
}
