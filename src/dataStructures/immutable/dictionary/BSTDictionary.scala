/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.dictionary

import dataStructures.immutable.searchTree.{BST, SearchTree}

object BSTDictionary {
  protected case class Rel[K, +V](key: K, value: V)

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

case class BSTDictionary[K, +V] private (private val tree: SearchTree[BSTDictionary.Rel[K, V]])(implicit ord: Ordering[K]) extends Dictionary[K, V] {
  def this()(implicit ord: Ordering[K]) {
    this(BST())
  }

  override def isEmpty: Boolean =
    tree.isEmpty

  override def size: Int =
    tree.size

  override def insert[VS >: V](key: K, value: VS): BSTDictionary[K, VS] =
    BSTDictionary(tree.insert(BSTDictionary.Rel(key, value)))

  override def delete(key: K): BSTDictionary[K, V] =
    BSTDictionary(tree.delete(BSTDictionary.withKey(key)))

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

  override def foldKeys[B](z: B)(f: (K, B) => B): B =
    tree.foldInOrder(z)((rel, z) => f(rel.key, z))

  override def foldValues[B](z: B)(f: (V, B) => B): B =
    tree.foldInOrder(z)((rel, z) => f(rel.value, z))

  override def foldKeysValues[B](z: B)(f: (K, V, B) => B): B =
    tree.foldInOrder(z)((rel, z) => f(rel.key, rel.value, z))

  override def toString: String =
    keysValues.mkString(this.getClass.getSimpleName + "(", ",", ")")
}
