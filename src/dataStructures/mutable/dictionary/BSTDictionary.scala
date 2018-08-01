/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.dictionary

import dataStructures.mutable.searchTree.BST

object BSTDictionary {
  protected case class Rel[K, V](key: K, value: V)

  implicit def ordKey2Rel[K, V](implicit ord: Ordering[K]) = new Ordering[Rel[K, V]] {
    override def compare(x: Rel[K, V], y: Rel[K, V]): Int =
      ord.compare(x.key, y.key)
  }

  private def withKey[K, V](key: K): Rel[K, V] =
    Rel(key, null.asInstanceOf[V])

  def empty[K, V](implicit ord: Ordering[K]): BSTDictionary[K, V] =
    new BSTDictionary()

  def apply[K, V]()(implicit ord: Ordering[K]): BSTDictionary[K, V] =
    new BSTDictionary()
}

class BSTDictionary[K,V] private (tree : BST[BSTDictionary.Rel[K, V]])(implicit ord: Ordering[K]) extends Dictionary[K,V] {
  def this()(implicit ord: Ordering[K]) {
    this(BST())
  }

  override def isEmpty: Boolean =
    tree.isEmpty

  override def size: Int =
    tree.size

  override def insert(key: K, value: V): Unit =
    tree.insert(BSTDictionary.Rel(key, value))

  override def delete(key: K): Unit =
    tree.delete(BSTDictionary.withKey(key))

  override def valueOf(key: K): Option[V] = tree.search(BSTDictionary.withKey(key)) match {
    case None =>
      None
    case Some(rel) =>
      Some(rel.value)
  }

  override def isElem(key: K): Boolean = tree.search(BSTDictionary.withKey(key)) match {
    case None =>
      false
    case Some(_) =>
      true
  }

  private class WithTreeIterable[T](extract : BSTDictionary.Rel[K,V] => T) extends Iterable[T] {
    override def iterator: Iterator[T] = new Iterator[T] {
      private val treeIt = tree.inOrder.iterator

      override def hasNext: Boolean =
        treeIt.hasNext

      override def next(): T =
        extract(treeIt.next())
    }
  }

  override def keys: Iterable[K] =
    new WithTreeIterable[K](rel => rel.key)

  override def values: Iterable[V] =
    new WithTreeIterable[V](rel => rel.value)

  override def keysValues: Iterable[(K, V)] =
    new WithTreeIterable[(K,V)](rel => (rel.key, rel.value))

  override def toString: String = {
    val sb = new StringBuilder(this.getClass.getSimpleName)
    sb.append('(')
    val it = keysValues.iterator
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
