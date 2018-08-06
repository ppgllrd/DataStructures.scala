/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait MergeableHeap[A, This <: Heap[A]] extends Heap[A] {
  def merge(that: This): This
}
