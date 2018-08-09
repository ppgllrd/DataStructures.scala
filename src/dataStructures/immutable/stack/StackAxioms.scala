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

  val factory: StackFactory[Elem]

  implicit val arbitraryElem: Arbitrary[Elem]

  implicit val arbitraryStack: Arbitrary[factory.Stack[Elem]]
}

class StackAxioms(test: Test) extends Properties("StackAxioms") {

  import test._

  private type Stack[A] = test.factory.Stack[A]

  private val empty = factory.empty

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

  override val factory = LinearStackFactory()

  implicit val arbitraryStack = LinearStack.arbitrary
}


object TestLinearStackInt extends StackAxioms(LinearStackTest[Int])

object TestLinearStackBool extends StackAxioms(LinearStackTest[Boolean])

object TestLinearStackTestData extends StackAxioms(LinearStackTest[TestData.Type])


case class StackOnListTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override val factory = ListStackFactory()

  implicit val arbitraryStack = ListStack.arbitrary
}


object TestStackOnListInt extends StackAxioms(StackOnListTest[Int])

object TestStackOnListBool extends StackAxioms(StackOnListTest[Boolean])

object TestStackOnListTestData extends StackAxioms(StackOnListTest[TestData.Type])
