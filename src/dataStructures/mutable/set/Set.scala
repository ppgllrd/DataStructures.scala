/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.set

trait Set[A] extends Iterable[A] {
  def isEmpty: Boolean

  def size: Int

  def insert(x: A): Unit

  def delete(x: A): Unit

  def isElem(x: A): Boolean

  def union(that: Set[A]): Unit =
    for (x <- that)
      insert(x)

  def intersection(that: Set[A]): Unit =
    for (x <- this)
      if (!that.isElem(x))
        delete(x)

  def difference(that: Set[A]): Unit =
    for (x <- that)
      delete(x)

  def copy: Set[A]
}
