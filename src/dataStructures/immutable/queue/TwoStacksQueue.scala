/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

import dataStructures.immutable.stack.{LinearStack, Stack}
import org.scalacheck.{Arbitrary, Gen}

class TwoStacksQueue[+A] private(val sFront: Stack[A], val sRear: Stack[A]) extends Queue[A] {
  def this() {
    this(LinearStack(), LinearStack())
  }

  // enforces this invariant: front stack is only empty if queue is empty
  private def enforceInvariant: TwoStacksQueue[A] =
    if (sFront.isEmpty && !sRear.isEmpty) {
      var stackFront = sFront
      var stackRear = sRear
      while (!stackRear.isEmpty) {
        stackFront = stackFront.push(stackRear.top)
        stackRear = stackRear.pop
      }
      new TwoStacksQueue[A](stackFront, stackRear)
    } else
      this

  override def isEmpty: Boolean =
    sFront.isEmpty

  override def enqueue[B >: A](x: B): TwoStacksQueue[B] =
    new TwoStacksQueue[B](sFront, sRear.push(x)).enforceInvariant

  override def first: A =
    if (isEmpty)
      throw EmptyQueueException("first on empty queue")
    else sFront.top

  override def dequeue: TwoStacksQueue[A] =
    if (isEmpty)
      throw EmptyQueueException("dequeue on empty queue")
    else
      new TwoStacksQueue(sFront.pop, sRear).enforceInvariant

  override def equals(o: scala.Any): Boolean = o match {
    case that: Queue[A] =>
      var opt: Option[Boolean] = None
      var q1 = this
      var q2 = that
      while (opt.isEmpty) {
        val e1 = q1.isEmpty
        val e2 = q2.isEmpty
        if (e1 && e2)
          opt = Some(true)
        else if (e1 != e2 || q1.first != q2.first)
          opt = Some(false)
        else {
          q1 = q1.dequeue
          q2 = q2.dequeue
        }
      }
      val Some(bool) = opt
      bool
    case _ =>
      false
  }

  override def toString: String = {
    val sb = new StringBuilder(this.getClass.getSimpleName)
    sb.append('(')
    var q = this
    var goOn = !q.isEmpty
    while (goOn) {
      sb.append(q.first)
      q = q.dequeue
      goOn = !q.isEmpty
      if (goOn)
        sb.append(',')
    }
    sb.append(')')
    sb.toString()
  }
}

case class TwoStacksQueueFactory[A]() extends QueueFactory[A] {
  override def empty: TwoStacksQueue[A] =
    new TwoStacksQueue[A]()
}

object TwoStacksQueue {
  def empty[A]: TwoStacksQueue[A] =
    new TwoStacksQueue[A]()

  def apply[A](): TwoStacksQueue[A] =
    new TwoStacksQueue[A]()

  def factory[A]: TwoStacksQueueFactory[A] =
    new TwoStacksQueueFactory()

  implicit def arbitrary[A](implicit a: Arbitrary[A]) = Arbitrary[TwoStacksQueue[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(TwoStacksQueue.empty[A])((x, s) => s.enqueue(x))
  }
}