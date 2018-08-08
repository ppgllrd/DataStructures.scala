/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

import org.scalacheck.{Arbitrary, Gen}

case class SkewHeapFactory[A](implicit ord: Ordering[A]) extends MergeableHeapFactory[A] {
  override type Heap = SkewHeap

  override def empty: SkewHeap =
    Empty

  override def singleton(elem: A): SkewHeap =
    Node(elem, Empty, Empty)

  sealed trait SkewHeap extends MergeableHeap[SkewHeap, A] {
    override def isEmpty: Boolean = this match {
      case Empty =>
        true
      case _ =>
        false
    }

    override def size: Int = this match {
      case Empty =>
        0
      case Node(_, lt, rt) =>
        1 + lt.size + rt.size
    }

    override def minElem: A = this match {
      case Empty =>
        throw EmptyHeapException("minElem on empty heap")
      case Node(x, _, _) =>
        x
    }

    /* Merge heaps along right spines enforcing heap order property.
     * Swap children for nodes in right spine of resulting heap.
     */
    override def merge(that: SkewHeap): SkewHeap =
      (this, that) match {
        case (Empty, h2) =>
          h2
        case (h1, Empty) =>
          h1
        case (h1@Node(x1, lt1, rt1), h2@Node(x2, lt2, rt2)) =>
          if (ord.compare(x1, x2) <= 0)
            Node(x1, h2.merge(rt1), lt1)
          else
            Node(x2, h1.merge(rt2), lt2)
      }

    override def insert(elem: A): SkewHeap =
      this.merge(singleton(elem))

    override def delMin: SkewHeap = this match {
      case Empty =>
        throw EmptyHeapException("delMin on empty heap")
      case Node(_, lt, rt) =>
        lt.merge(rt)
    }

    def mkString: String = {
      val sb = new StringBuilder("SkewHeap(")
      var h = this
      var goOn = !h.isEmpty
      while (goOn) {
        sb.append(h.minElem)
        h = h.delMin
        goOn = !h.isEmpty
        if (goOn)
          sb.append(", ")
      }
      sb.append(')')
      sb.toString
    }

    override def toString: String = {
      val sb = new StringBuilder

      def aux(bst: SkewHeap): Unit = bst match {
        case Empty =>
          sb.append("Empty")
        case Node(x, lt, rt) =>
          sb.append("Node(")
          sb.append(x)
          sb.append(", ")
          aux(lt)
          sb.append(", ")
          aux(rt)
          sb.append(')')
      }

      aux(this)
      sb.toString
    }
  }

  private case class Node(elem: A, lt: SkewHeap, rt: SkewHeap) extends SkewHeap

  private case object Empty extends SkewHeap

}

object SkewHeap {
  def empty[A](implicit ord: Ordering[A]): SkewHeapFactory[A]#SkewHeap =
    factory[A].empty

  def apply[A]()(implicit ord: Ordering[A]): SkewHeapFactory[A]#SkewHeap =
    factory[A].empty

  def factory[A](implicit ord: Ordering[A]): SkewHeapFactory[A] =
    new SkewHeapFactory[A]()

  implicit def arbitrary[A](implicit a: Arbitrary[A], ord: Ordering[A]) = Arbitrary[SkewHeapFactory[A]#SkewHeap] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(empty[A](ord))((x, bst) => bst.insert(x))
  }
}
