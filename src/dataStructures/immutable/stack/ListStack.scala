/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

import org.scalacheck.{Arbitrary, Gen}

case class ListStack[+A] private(private val xs: List[A]) extends Stack[A] {
  def this() {
    this(List())
  }

  override def isEmpty: Boolean =
    xs.isEmpty

  override def push[B >: A](x: B): ListStack[B] =
    ListStack(x :: xs)

  override def top: A =
    if (isEmpty)
      throw EmptyStackException("top on empty stack")
    else
      xs.head

  override def pop: ListStack[A] =
    if (isEmpty)
      throw EmptyStackException("pop on empty stack")
    else
      ListStack(xs.tail)

  override def toString: String =
    xs.mkString(this.getClass.getSimpleName + "(", ",", ")")
}

object ListStack {
  def empty[A]: ListStack[A] =
    new ListStack()

  def apply[A](): ListStack[A] =
    new ListStack()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[ListStack[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(ListStack.empty[A])((x, s) => s.push(x))
  }
}