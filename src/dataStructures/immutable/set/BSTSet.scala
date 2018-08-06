/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.set

import dataStructures.immutable.searchTree.{BST, BSTFactory, SearchTree}
import org.scalacheck.{Arbitrary, Gen}

case class BSTSet[A] private(private val tree: SearchTree[A])(implicit ord: Ordering[A]) extends Set[A] {
  def this()(implicit ord: Ordering[A]) {
    this(BST.empty)
  }

  override def isEmpty: Boolean =
    tree.isEmpty

  override def size: Int =
    tree.size

  override def insert(x: A): BSTSet[A] =
    BSTSet(tree.insert(x))

  override def delete(x: A): BSTSet[A] =
    BSTSet(tree.delete(x))

  override def isElem(x: A): Boolean =
    tree.isElem(x)

  override def fold[B](z: B)(f: (A, B) => B): B =
    tree.foldInOrder(z)(f)

  override def equals(o: scala.Any): Boolean = o match {
    case that: Set[A] =>
      this.size == that.size && this.difference(that).isEmpty
    case _ =>
      false
  }

  override def toString: String =
    fold(List[A]())(_ :: _).mkString(this.getClass.getSimpleName + "(", ",", ")")
}

case class BSTSetFactory[A](implicit ord: Ordering[A]) extends BSTSet.Factory[A]

object BSTSet {
  def empty[A](implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()

  def apply[A]()(implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()

  class Factory[A](implicit ord: Ordering[A]) extends SetFactory[A] {
    private val bstFactory = new BSTFactory[A]()(ord)

    override def empty: BSTSet[A] =
      new BSTSet[A](bstFactory.empty)
  }

  def factory[A](implicit ord: Ordering[A]): BSTSet.Factory[A] =
    new Factory[A]()

  implicit def arbitrary[A](implicit a: Arbitrary[A], ord: Ordering[A]) = Arbitrary[BSTSet[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(BSTSet.empty[A])((x, s) => s.insert(x))
  }
}
