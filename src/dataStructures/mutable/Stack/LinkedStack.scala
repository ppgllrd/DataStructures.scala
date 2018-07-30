/******************************************************************************
 * Data Structures in Scala
 *
 * Pepe Gallardo, 2018
 *****************************************************************************/

package dataStructures.mutable.Stack

class LinkedStack[A] extends Stack[A] {
  private case class Node[E](elem : E, next : Node[E])

  private var topNode : Node[A] = null

  override def isEmpty: Boolean =
    topNode == null

  override def push(x: A): Unit =
    topNode = Node(x, topNode)

  override def pop: Unit =
    if(isEmpty)
      throw EmptyStackException("pop on empty stack")
    else
      topNode = topNode.next

  override def top: A =
    if(isEmpty)
      throw EmptyStackException("top on empty stack")
    else
      topNode.elem
}
