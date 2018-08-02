/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

import dataStructures.util.TestData
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Arbitrary, Properties}

trait Test {
  type Elem // Base type

  type Queue[A] <: dataStructures.immutable.queue.Queue[A] // Queue implementation

  def empty: Queue[Elem] // Returns an empty queue

  implicit val arbitraryElem: Arbitrary[Elem]

  implicit val arbitraryQueue: Arbitrary[Queue[Elem]]
}

class QueueAxioms(test: Test) extends Properties("QueueAxioms") {

  import test._

  private type Queue[A] = test.Queue[A]

  property("isEmpty empty") =
    empty.isEmpty

  property("not isEmpty enqueue") = forAll { (q: Queue[Elem], x: Elem) =>
    !q.enqueue(x).isEmpty
  }

  property("first enqueue empty") = forAll { x: Elem =>
    empty.enqueue(x).first == x
  }

  property("first enqueue non-empty") = forAll { (q: Queue[Elem], x: Elem) =>
    !q.isEmpty ==> (q.enqueue(x).first == q.first)
  }

  property("enqueue dequeue empty") = forAll { x: Elem =>
    empty.enqueue(x).dequeue == empty
  }

  property("enqueue dequeue commute non-empty") = forAll { (q: Queue[Elem], x: Elem) =>
    !q.isEmpty ==> (q.enqueue(x).dequeue == q.dequeue.enqueue(x))
  }
}


case class LinearQueueTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override type Queue[A] = LinearQueue[A]

  override def empty = LinearQueue()

  implicit val arbitraryQueue = LinearQueue.arbitrary
}


object TestLinearQueueInt extends QueueAxioms(LinearQueueTest[Int])

object TestLinearQueueBool extends QueueAxioms(LinearQueueTest[Boolean])

object TestLinearQueueTestData extends QueueAxioms(LinearQueueTest[TestData.Type])


case class TwoStacksQueueTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override type Queue[A] = TwoStacksQueue[A]

  override def empty = TwoStacksQueue()

  implicit val arbitraryQueue = TwoStacksQueue.arbitrary
}


object TestTwoStacksQueueInt extends QueueAxioms(TwoStacksQueueTest[Int])

object TestTwoStacksQueueBool extends QueueAxioms(TwoStacksQueueTest[Boolean])

object TestTwoStacksQueueTestData extends QueueAxioms(TwoStacksQueueTest[TestData.Type])
