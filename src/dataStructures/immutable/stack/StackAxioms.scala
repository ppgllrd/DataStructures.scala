/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.stack

import dataStructures.util.TestData
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

trait Test {
  type Elem // Base type

  type Stack[A] <: dataStructures.immutable.stack.Stack[A] // Stack implementation

  def empty: Stack[Elem] // Returns an empty stack

  implicit val arbitraryElem: Arbitrary[Elem]

  implicit val arbitraryStack: Arbitrary[Stack[Elem]]
}

class StackAxioms(test: Test) extends Properties("StackAxioms") {

  import test._

  private type Stack[A] = test.Stack[A]

  property("isEmpty empty") =
    empty.isEmpty

  property("not isEmpty push") = forAll { (s: Stack[Elem], x: Elem) =>
    !s.push(x).isEmpty
  }

  property("top push") = forAll { (s: Stack[Elem], x: Elem) =>
    s.push(x).top == x
  }

  property("pop push") = forAll { (s: Stack[Elem], x: Elem) =>
    s.push(x).pop == s
  }
}

case class LinearStackTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override type Stack[A] = LinearStack[A]

  override def empty = LinearStack()

  implicit val arbitraryStack = LinearStack.arbitrary
}


object TestLinearStackInt extends StackAxioms(LinearStackTest[Int])

object TestLinearStackBool extends StackAxioms(LinearStackTest[Boolean])

object TestLinearStackTestData extends StackAxioms(LinearStackTest[TestData.Type])


case class StackOnListTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override type Stack[A] = StackOnList[A]

  override def empty = StackOnList()

  implicit val arbitraryStack = StackOnList.arbitrary
}


object TestStackOnListInt extends StackAxioms(StackOnListTest[Int])

object TestStackOnListBool extends StackAxioms(StackOnListTest[Boolean])

object TestStackOnListTestData extends StackAxioms(StackOnListTest[TestData.Type])
