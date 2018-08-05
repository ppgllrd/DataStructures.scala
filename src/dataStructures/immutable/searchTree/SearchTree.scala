/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.searchTree

trait SearchTree[A] {
  def isEmpty: Boolean

  def size: Int

  def search(e: A): Option[A]

  def isElem(e: A): Boolean

  def insert(e: A): SearchTree[A]

  def delete(e: A): SearchTree[A]

  def minim: A

  def maxim: A

  def deleteMinim: SearchTree[A]

  def deleteMaxim: SearchTree[A]

  def foldInOrder[B](z: B)(f: (A, B) => B): B

  def foldPreOrder[B](z: B)(f: (A, B) => B): B

  def foldPostOrder[B](z: B)(f: (A, B) => B): B
}
