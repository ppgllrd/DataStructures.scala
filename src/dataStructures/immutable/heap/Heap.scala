/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

trait HeapLike[+This, A] {
  def isEmpty: Boolean

  def size: Int

  def insert(x: A): This

  def minElem: A

  def delMin: This
}

trait Heap[A] extends HeapLike[Heap[A], A]