/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

import org.scalacheck.{Arbitrary, Gen}

sealed trait LinearStack[+A] extends Stack[A] with StackLike[LinearStack, A] {
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

case class LinearStackFactory[A]() extends StackFactory[A] {
  override type Stack[A] = LinearStack[A]

  override def empty: LinearStack[A] =
    Empty
}

object LinearStack {
  def empty[A]: LinearStack[A] =
    Empty

  def apply[A](): LinearStack[A] =
    Empty

  def factory[A]: LinearStackFactory[A] =
    new LinearStackFactory()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[LinearStack[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(LinearStack.empty[A])((x, s) => s.push(x))
  }
}

