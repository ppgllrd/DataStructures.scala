/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

sealed trait LinearStack[+A] extends Stack[A] {
  override def isEmpty: Boolean = this match {
    case Empty =>
      true
    case Node(_, _) =>
      false
  }

  override def push[B >: A](x: B): LinearStack[B] =
    Node(x, this)

  override def top: A = this match {
    case Empty =>
      throw EmptyStackException("top on empty stack")
    case Node(x, _) =>
      x
  }

  override def pop: LinearStack[A] = this match {
    case Empty =>
      throw EmptyStackException("pop on empty stack")
    case Node(_, s) =>
      s
  }

  private def toList: List[A] = this match {
    case Empty =>
      List()
    case Node(x, s) =>
      x :: s.toList
  }

  override def toString: String =
    this.toList.mkString("LinearStack(", ",", ")")
}

private case object Empty extends LinearStack[Nothing]

private case class Node[A](x: A, next: LinearStack[A]) extends LinearStack[A]

object LinearStack {
  def empty[A]: LinearStack[A] =
    Empty

  def apply[A](): LinearStack[A] =
    Empty
}