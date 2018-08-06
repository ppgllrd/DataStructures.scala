/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait HeapFactory[A] {
  def empty: Heap[A]

  def singleton(x: A): Heap[A]
}
