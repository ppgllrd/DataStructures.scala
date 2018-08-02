/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.set

import org.scalacheck.{Arbitrary, Gen}

object ListSet {
  def empty[A](): ListSet[A] =
    new ListSet[A]()

  def apply[A](): ListSet[A] =
    new ListSet[A]()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[ListSet[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(ListSet.empty[A])((x, s) => s.insert(x))
  }
}

class ListSet[A] private(elems: List[A]) extends Set[A] {
  def this() {
    this(List())
  }

  override def isEmpty: Boolean =
    elems.isEmpty

  override def size: Int =
    elems.length

  override def insert(x: A): ListSet[A] =
    if (elems.contains(x))
      this
    else
      new ListSet(x :: elems)

  override def delete(x: A): ListSet[A] =
    new ListSet(elems.diff(List(x)))

  override def isElem(x: A): Boolean =
    elems.contains(x)

  override def fold[B](z: B)(f: (A, B) => B): B =
    elems.foldRight(z)(f)

  override def equals(o: scala.Any): Boolean = o match {
    case that: Set[A] =>
      this.size == that.size && this.difference(that).isEmpty
    case _ =>
      false
  }

  override def toString: String =
    elems.mkString(this.getClass.getSimpleName + "(", ",", ")")
}
