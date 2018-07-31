/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.dictionary

trait Dictionary[K, +V] {
  def isEmpty: Boolean

  def size: Int

  def insert[VS >: V](key: K, value: VS): Dictionary[K, VS]

  def delete(key: K): Dictionary[K, V]

  def valueOf(key: K): Option[V]

  def isElem(key: K): Boolean

  def foldKeys[B](z: B)(f: (K, B) => B): B

  def foldValues[B](z: B)(f: (V, B) => B): B

  def foldKeysValues[B](z: B)(f: (K, V, B) => B): B

  def keys: Seq[K] =
    foldKeys(List[K]())(_ :: _)

  def values: Seq[V] =
    foldValues(List[V]())(_ :: _)

  def keysValues: Seq[(K, V)] =
    foldKeysValues(List[(K, V)]())((k, v, xs) => (k, v) :: xs)
}
