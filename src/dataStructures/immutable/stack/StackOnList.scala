/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

import org.scalacheck.{Arbitrary, Gen}

case class StackOnList[+A] private (private val xs: List[A]) extends Stack[A] {
  def this() {
    this(List())
  }

  override def isEmpty: Boolean =
    xs.isEmpty

  override def push[B >: A](x: B): StackOnList[B] =
    StackOnList(x :: xs)

  override def top: A =
    if (isEmpty)
      throw EmptyStackException("top on empty stack")
    else
      xs.head

  override def pop: StackOnList[A] =
    if (isEmpty)
      throw EmptyStackException("pop on empty stack")
    else
      StackOnList(xs.tail)

  override def toString: String =
    xs.mkString(this.getClass.getSimpleName + "(", ",", ")")
}

object StackOnList {
  def empty[A]: StackOnList[A] =
    new StackOnList()

  def apply[A](): StackOnList[A] =
    new StackOnList()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[StackOnList[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(StackOnList.empty[A])((x, s) => s.push(x))
  }
}