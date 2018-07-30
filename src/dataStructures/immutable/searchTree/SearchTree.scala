/******************************************************************************
 * Data Structures in Scala
 *
 * Pepe Gallardo, 2018
 *****************************************************************************/

package dataStructures.immutable.searchTree

trait SearchTree[+A] {
  def isEmpty: Boolean

  def size: Int

  def search[B >: A](e: B)(implicit ord: Ordering[B]): Option[A]

  def isElem[B >: A](e: B)(implicit ord: Ordering[B]): Boolean

  def insert[B >: A](e: B)(implicit ord: Ordering[B]): BST[B]

  def delete[B >: A](e: B)(implicit ord: Ordering[B]): BST[A]

  def minim: A

  def maxim: A

  def deleteMinim: BST[A]

  def deleteMaxim: BST[A]

  def foldInOrder[B](z: B)(f: (A, B) => B): B

  def foldPreOrder[B](z: B)(f: (A, B) => B): B

  def foldPostOrder[B](z: B)(f: (A, B) => B): B
}
