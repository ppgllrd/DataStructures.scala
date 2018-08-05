package dataStructures.immutable.heap

trait Heap[+A] {
  def isEmpty: Boolean

  def size: Int

  def insert[B >: A](x: B)(implicit ord: Ordering[B]): Heap[B]

  def minElem: A

  def delMin[B >: A](implicit ord: Ordering[B]): Heap[B]
}
