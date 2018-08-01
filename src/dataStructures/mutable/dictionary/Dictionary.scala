/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.dictionary

trait Dictionary[K, V] {
  def isEmpty: Boolean

  def size: Int

  def insert(key: K, value: V): Unit

  def delete(key: K): Unit

  def valueOf(key: K): Option[V]

  def isElem(key: K): Boolean

  def keys: Iterable[K]

  def values: Iterable[V]

  def keysValues: Iterable[(K, V)]
}
