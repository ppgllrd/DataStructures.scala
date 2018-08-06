/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.stack

trait StackFactory[A] {
  def empty: Stack[A]
}
