/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.set

import dataStructures.mutable.searchTree.BST

object BSTSet {
  def apply[A]()(implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()

  def empty[A]()(implicit ord: Ordering[A]): BSTSet[A] =
    new BSTSet[A]()
}

class BSTSet[A] private(private val tree: BST[A])(implicit ord: Ordering[A]) extends Set[A] {
  def this()(implicit ord: Ordering[A]) {
    this(BST())
  }

  override def isEmpty: Boolean =
    tree.isEmpty

  override def size: Int =
    tree.size

  override def insert(x: A): Unit =
    tree.insert(x)

  override def delete(x: A): Unit =
    tree.delete(x)

  override def isElem(x: A): Boolean =
    tree.isElem(x)

  override def iterator: Iterator[A] =
    tree.inOrderIt

  override def copy: BSTSet[A] =
    new BSTSet(tree.copy)

  override def toString(): String = {
    val sb = new StringBuilder(this.getClass.getSimpleName)
    sb.append('(')
    val it = iterator
    var goOn = it.hasNext
    while (goOn) {
      sb.append(it.next())
      goOn = it.hasNext
      if (goOn)
        sb.append(',')
    }
    sb.append(')')
    sb.toString
  }
}
