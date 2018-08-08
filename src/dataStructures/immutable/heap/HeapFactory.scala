/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait HeapFactory[A] {
  type Heap <: dataStructures.immutable.heap.Heap[Heap, A]

  def empty: Heap

  def singleton(x: A): Heap
}
