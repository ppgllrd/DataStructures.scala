/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.set

trait Set[A] {
  def isEmpty: Boolean

  def size: Int

  def insert(x: A): Set[A]

  def delete(x: A): Set[A]

  def isElem(x: A): Boolean

  def fold[B](z: B)(f: (A, B) => B): B

  def union(that: Set[A]): Set[A] =
    that.fold(this)((x, s) => s.insert(x))

  def intersection(that: Set[A]): Set[A] =
    that.fold(that)((x, s) => if (this.isElem(x)) s else s.delete(x))

  def difference(that: Set[A]): Set[A] =
    that.fold(this)((x, s) => s.delete(x))
}
