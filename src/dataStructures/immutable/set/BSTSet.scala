/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.set

import dataStructures.immutable.searchTree.{BST, SearchTree}

object BSTSet {
  def empty[A]()(implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()

  def apply[A]()(implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()
}

case class BSTSet[A](private val tree: SearchTree[A])(implicit ord: Ordering[A]) extends Set[A] {
  def this()(implicit ord: Ordering[A]) {
    this(BST())
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

  override def toString: String =
    fold(List[A]())(_ :: _).mkString(this.getClass.getSimpleName + "(", ",", ")")
}
