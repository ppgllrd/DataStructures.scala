/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

trait StackLike[+This[+ _], +A] {
  def isEmpty: Boolean

  def push[B >: A](x: B): This[B]

  def top: A

  def pop: This[A]
}

trait Stack[+A] extends StackLike[Stack, A]