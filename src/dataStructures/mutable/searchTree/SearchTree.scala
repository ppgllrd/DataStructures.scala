/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.searchTree

trait SearchTree[A] {
  def isEmpty: Boolean

  def size: Int

  def search(e: A): Option[A]

  def isElem(e: A): Boolean

  def insert(e: A): Unit

  def delete(e: A): Unit

  def minim: A

  def maxim: A

  def deleteMinim: Unit

  def deleteMaxim: Unit

  def copy: SearchTree[A]

  def inOrderIt: Iterator[A]

  def inOrder: Iterable[A]

  def preOrderIt: Iterator[A]

  def preOrder: Iterable[A]

  def postOrderIt: Iterator[A]

  def postOrder: Iterable[A]
}
