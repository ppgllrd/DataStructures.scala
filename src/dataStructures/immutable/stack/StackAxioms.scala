/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

class StackAxioms[A, S[_] <: Stack[_]](empty: S[A])(implicit val a: Arbitrary[A], implicit val s: Arbitrary[S[A]]) extends Properties("StackAxioms") {
  property("top push") = forAll { (s: S[A], x: A) =>
    s.push(x).top == x
  }

  property("pop push") = forAll { (s: S[A], x: A) =>
    s.push(x).pop == s
  }

  property("isEmpty empty") =
    empty.isEmpty

  property("not isEmpty push") = forAll { (s: S[A], x: A) =>
    !s.push(x).isEmpty
  }
}

import dataStructures.immutable.stack.LinearStack.arbitrary

object TestLinearStackInt extends StackAxioms[Int, LinearStack](LinearStack.empty)

object TestLinearStackBoolean extends StackAxioms[Boolean, LinearStack](LinearStack.empty)


import dataStructures.immutable.stack.StackOnList.arbitrary

object TestStackOnListInt extends StackAxioms[Int, StackOnList](StackOnList.empty)

object TestStackOnListChar extends StackAxioms[Char, StackOnList](StackOnList.empty)
