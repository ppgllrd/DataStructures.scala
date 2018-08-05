package dataStructures.immutable.heap

object WBLH {
  def apply[A](): WBLH[A] =
    Empty

  def empty[A](): WBLH[A] =
    Empty

  def singleton[A](elem: A): WBLH[A] =
    Node(elem, 1, Empty, Empty)

  // smart constructor: sets heaviest tree as left child
  private def node[A](elem: A, h1: WBLH[A], h2: WBLH[A]): WBLH[A] = {
    val w1 = h1.weight
    val w2 = h2.weight
    val weight = 1 + (w1 max w2)
    if (w1 >= w2)
      Node(elem, weight, h1, h2)
    else
      Node(elem, weight, h2, h1)
  }
}

sealed trait WBLH[+A] extends MergeableHeap[A, WBLH] {
  private def weight: Int = this match {
    case Empty =>
      0
    case Node(_, w, _, _) =>
      w
  }

  def isEmpty: Boolean = this match {
    case Empty =>
      true
    case _ =>
      false
  }

  def size: Int =
    weight

  def minElem: A = this match {
    case Empty =>
      throw EmptyHeapException("minElem on empty heap")
    case Node(x, _, _, _) =>
      x
  }

  override def merge[B >: A](that: WBLH[B])(implicit ord: Ordering[B]): WBLH[B] =
    (this, that) match {
      case (Empty, h2) =>
        h2
      case (h1, Empty) =>
        h1
      case (h1@Node(x1, _, lt1, rt1), h2@Node(x2, _, lt2, rt2)) =>
        if (ord.compare(x1, x2) <= 0)
          WBLH.node(x1, lt1, rt1.merge(h2))
        else
          WBLH.node(x2, lt2, rt2.merge(h1))
    }

  def insert[B >: A](elem: B)(implicit ord: Ordering[B]): WBLH[B] =
    this.merge(WBLH.singleton(elem))

  def delMin[B >: A](implicit ord: Ordering[B]): WBLH[B] = this match {
    case Empty =>
      throw EmptyHeapException("delMin on empty heap")
    case Node(_, _, lt, rt) =>
      lt.merge(rt.asInstanceOf[WBLH[B]]) // note this casting :-(
  }

  def mkString[B >: A](prefix: String, infix: String, suffix: String)(implicit ord: Ordering[B]): String = {
    val sb = new StringBuilder(prefix)
    var h = this.asInstanceOf[WBLH[B]] // note this casting :-(
    var goOn = !h.isEmpty
    while (goOn) {
      sb.append(h.minElem)
      h = h.delMin
      goOn = !h.isEmpty
      if (goOn)
        sb.append(infix)
    }
    sb.append(suffix)
    sb.toString
  }
}

private case class Node[+A](elem: A, weight: Int, lt: WBLH[A], rt: WBLH[A]) extends WBLH[A]

private case object Empty extends WBLH[Nothing]


object MergeableWBLHOps extends MergeableHeapOps[WBLH] {
  override def singleton[A](elem: A): WBLH[A] =
    WBLH.singleton(elem)
}


object Demo extends App {
  var h1 = WBLH[Int]()

  h1 = h1.insert(5)
  h1 = h1.insert(7)
  h1 = h1.insert(3)
  h1 = h1.insert(9)
  h1 = h1.insert(1)

  println(h1)

  println(h1.minElem)

  h1 = h1.delMin

  println(h1.mkString("WBLH(", ",", ")"))


  var xs = Array(10, 3, 2, 10, 1, 9, 7)
  MergeableWBLHOps.heapSort(xs)
  println(xs.mkString(" "))
}