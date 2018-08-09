/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.heap

import org.scalacheck.{Arbitrary, Gen}

case class WBLHFactory[A](implicit ord: Ordering[A]) extends MergeableHeapFactory[A] {
  override type Heap = WBLH

  override def empty: Heap =
    Empty

  override def singleton(elem: A): Heap =
    Node(elem, 1, Empty, Empty)

  // smart constructor: sets heaviest tree as left child
  private def node(elem: A, h1: WBLH, h2: WBLH): WBLH = {
    val w1 = h1.weight
    val w2 = h2.weight
    val weight = 1 + (w1 max w2)
    if (w1 >= w2)
      Node(elem, weight, h1, h2)
    else
      Node(elem, weight, h2, h1)
  }

  sealed trait WBLH extends MergeableHeap[A] with MergeableHeapLike[WBLH, A] {
    def weight: Int = this match {
      case Empty =>
        0
      case Node(_, w, _, _) =>
        w
    }

    override def isEmpty: Boolean = this match {
      case Empty =>
        true
      case _ =>
        false
    }

    override def size: Int =
      weight

    override def minElem: A = this match {
      case Empty =>
        throw EmptyHeapException("minElem on empty heap")
      case Node(x, _, _, _) =>
        x
    }

    override def merge[That >: WBLH](that: That): That =
      (this, that) match {
        case (Empty, h2) =>
          h2
        case (h1, Empty) =>
          h1
        case (h1@Node(x1, _, lt1, rt1), h2@Node(x2, _, lt2, rt2)) =>
          if (ord.compare(x1, x2) <= 0)
            node(x1, lt1, rt1.merge(h2))
          else
            node(x2, lt2, rt2.merge(h1))
      }

    override def insert(elem: A): WBLH =
      this.merge(singleton(elem))

    override def delMin: WBLH = this match {
      case Empty =>
        throw EmptyHeapException("delMin on empty heap")
      case Node(_, _, lt, rt) =>
        lt.merge(rt)
    }

    def mkString: String = {
      val sb = new StringBuilder("WBLH(")
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

      def aux(bst: WBLH): Unit = bst match {
        case Empty =>
          sb.append("Empty")
        case Node(x, _, lt, rt) =>
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

  private case class Node(elem: A, wght: Int, lt: WBLH, rt: WBLH) extends WBLH

  private case object Empty extends WBLH

}

object WBLH {
  def empty[A](implicit ord: Ordering[A]): WBLHFactory[A]#WBLH =
    factory[A].empty

  def apply[A]()(implicit ord: Ordering[A]): WBLHFactory[A]#WBLH =
    factory[A].empty

  def factory[A](implicit ord: Ordering[A]): WBLHFactory[A] =
    new WBLHFactory[A]()

  implicit def arbitrary[A](implicit a: Arbitrary[A], ord: Ordering[A]) = Arbitrary[WBLHFactory[A]#WBLH] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(empty[A](ord))((x, bst) => bst.insert(x))
  }
}
