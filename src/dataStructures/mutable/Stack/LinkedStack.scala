/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.Stack

object LinkedStack {

  private case class Node[E](elem: E, next: Node[E]) // this is a static class

  def apply[A](): LinkedStack[A] =
    new LinkedStack[A]()
}

class LinkedStack[A] extends Stack[A] {
  private var topNode: LinkedStack.Node[A] = null

  override def isEmpty: Boolean =
    topNode == null

  override def push(x: A): Unit =
    topNode = LinkedStack.Node(x, topNode)

  override def pop(): Unit =
    if (isEmpty)
      throw EmptyStackException("pop on empty stack")
    else
      topNode = topNode.next

  override def top: A =
    if (isEmpty)
      throw EmptyStackException("top on empty stack")
    else
      topNode.elem

  override def toString: String = {
    val sb = new StringBuilder(this.getClass.getSimpleName)
    sb.append('(')
    var node = topNode
    while (node != null && node.next != null) {
      sb.append(node.elem)
      sb.append(',')
      node = node.next
    }
    if (node != null)
      sb.append(node.elem)
    sb.append(')')
    sb.toString
  }
}
