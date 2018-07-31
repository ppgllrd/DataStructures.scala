/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.stack

trait Stack[A] {
  def isEmpty: Boolean

  def push(x: A): Unit

  def pop(): Unit

  def top: A
}
