/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.searchTree

trait SearchTree[+A] {
  def isEmpty: Boolean

  def size: Int

  def search[B >: A](e: B)(implicit ord: Ordering[B]): Option[A]

  def isElem[B >: A](e: B)(implicit ord: Ordering[B]): Boolean

  def insert[B >: A](e: B)(implicit ord: Ordering[B]): SearchTree[B]

  def delete[B >: A](e: B)(implicit ord: Ordering[B]): SearchTree[A]

  def minim: A

  def maxim: A

  def deleteMinim: SearchTree[A]

  def deleteMaxim: SearchTree[A]

  def foldInOrder[B](z: B)(f: (A, B) => B): B

  def foldPreOrder[B](z: B)(f: (A, B) => B): B

  def foldPostOrder[B](z: B)(f: (A, B) => B): B
}
