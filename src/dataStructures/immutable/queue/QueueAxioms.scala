/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.{Arbitrary, Properties}

class QueueAxioms[A, Q[_] <: Queue[_]](empty: Q[A])(implicit val a: Arbitrary[A], implicit val s: Arbitrary[Q[A]]) extends Properties("QueueAxioms") {
  property("first enqueue empty") = forAll { x: A =>
    empty.enqueue(x).first == x
  }

  property("first enqueue non-empty") = forAll { (q : Q[A], x: A) =>
    !q.isEmpty ==> (q.enqueue(x).first == q.first)
  }

  property("enqueue dequeue empty") = forAll { x: A =>
    empty.enqueue(x).dequeue == empty
  }

  property("enqueue dequeue commute non-empty") = forAll { (q : Q[A], x: A) =>
    !q.isEmpty ==> (q.enqueue(x).dequeue == q.dequeue.enqueue(x))
  }

  property("isEmpty empty") =
    empty.isEmpty

  property("not isEmpty enqueue") = forAll { (q: Q[A], x: A) =>
    !q.enqueue(x).isEmpty
  }
}

import dataStructures.immutable.queue.LinearQueue.arbitrary

object TestLinearQueueInt extends QueueAxioms[Int, LinearQueue](LinearQueue.empty)

object TestLinearQueueBoolean extends QueueAxioms[Boolean, LinearQueue](LinearQueue.empty)
