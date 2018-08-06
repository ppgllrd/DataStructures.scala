/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

import org.scalacheck.{Arbitrary, Gen}

sealed trait LinearQueue[+A] extends Queue[A] {
  override def isEmpty: Boolean = this match {
    case Empty =>
      true
    case Node(_, _) =>
      false
  }

  override def enqueue[B >: A](x: B): LinearQueue[B] = this match {
    case Empty =>
      Node(x, Empty)
    case Node(y, q) =>
      Node(y, q.enqueue(x))
  }

  override def first: A = this match {
    case Empty =>
      throw EmptyQueueException("first on empty queue")
    case Node(x, _) =>
      x
  }

  override def dequeue: LinearQueue[A] = this match {
    case Empty =>
      throw EmptyQueueException("dequeue on empty queue")
    case Node(_, q) =>
      q
  }

  private def toList: List[A] = this match {
    case Empty =>
      List()
    case Node(x, q) =>
      x :: q.toList
  }

  override def toString: String =
    this.toList.mkString("LinearQueue(", ",", ")")
}

private case object Empty extends LinearQueue[Nothing]

private case class Node[A](x: A, next: LinearQueue[A]) extends LinearQueue[A]

case class LinearQueueFactory[A]() extends QueueFactory[A] {
  override def empty: LinearQueue[A] =
    Empty
}

object LinearQueue {
  def empty[A]: LinearQueue[A] =
    Empty

  def apply[A](): LinearQueue[A] =
    Empty

  def factory[A]: LinearQueueFactory[A] =
    new LinearQueueFactory()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[LinearQueue[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(LinearQueue.empty[A])((x, s) => s.enqueue(x))
  }
}