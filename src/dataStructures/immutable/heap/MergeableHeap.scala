package dataStructures.immutable.heap

trait MergeableHeap[+A, This[T] <: Heap[T]] extends Heap[A] {
  def merge[B >: A](that: This[B])(implicit ord: Ordering[B]): This[B]
}
