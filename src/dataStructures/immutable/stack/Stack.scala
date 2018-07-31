/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

trait Stack[+A] {
  def isEmpty: Boolean

  def push[B >: A](x: B): Stack[B]

  def top: A

  def pop: Stack[A]
}
