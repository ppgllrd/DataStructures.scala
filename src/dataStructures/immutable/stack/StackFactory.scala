/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

trait StackFactory[A] {
  type Stack[+A] <: dataStructures.immutable.stack.Stack[A]
    with dataStructures.immutable.stack.StackLike[Stack, A]

  def empty: Stack[A]
}
