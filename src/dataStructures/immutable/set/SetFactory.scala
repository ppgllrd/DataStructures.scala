/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/
package dataStructures.immutable.set

trait SetFactory[A] {
  def empty: Set[A]
}
