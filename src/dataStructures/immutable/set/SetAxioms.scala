/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.set

import dataStructures.util.TestData
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Arbitrary, Properties}

trait Test {
  type Elem // Base type

  type Set[A] <: dataStructures.immutable.set.Set[A] // Set implementation

  def empty: Set[Elem] // Returns an empty set

  implicit val arbitraryElem: Arbitrary[Elem]

  implicit val arbitrarySet: Arbitrary[Set[Elem]]
}

class SetAxioms(test: Test) extends Properties("SetAxioms") {

  import test._

  private type Set[A] = test.Set[A]

  property("isEmpty empty") =
    empty.isEmpty

  property("not isEmpty insert") = forAll { (s: Set[Elem], x: Elem) =>
    !s.insert(x).isEmpty
  }

  property("not isElem empty") = forAll { x: Elem =>
    !empty.isElem(x)
  }

  property("isElem insert") = forAll { (s: Set[Elem], x: Elem, y: Elem) =>
    s.insert(y).isElem(x) == (x == y) || s.isElem(x)
  }

  property("size empty") =
    empty.size == 0

  property("size insert isElem") = forAll { (s: Set[Elem], x: Elem) =>
    s.isElem(x) ==> (s.insert(x).size == s.size)
  }

  property("size insert not-isElem") = forAll { (s: Set[Elem], x: Elem) =>
    !s.isElem(x) ==> (s.insert(x).size == 1 + s.size)
  }

  property("delete empty") = forAll { x: Elem =>
    empty.delete(x) == empty
  }

  property("delete non-empty equal") = forAll { (s: Set[Elem], x: Elem) =>
    val y = x; s.insert(y).delete(x) == s.delete(x)
  }

  property("delete non-empty non-equal") = forAll { (s: Set[Elem], x: Elem, y: Elem) =>
    x != y ==> (s.insert(y).delete(x) == s.delete(x).insert(y))
  }

  property("union empty") = forAll { s: Set[Elem] =>
    s.union(empty) == s
  }

  property("union non-empty") = forAll { (s1: Set[Elem], s2: Set[Elem], x: Elem) =>
    s1.union(s2.insert(x)) == s1.union(s2).insert(x)
  }

  property("intersection empty") = forAll { s: Set[Elem] =>
    s.intersection(empty) == empty
  }

  property("intersection isElem non-empty") = forAll { (s1: Set[Elem], s2: Set[Elem], x: Elem) =>
    s1.isElem(x) ==> (s1.intersection(s2.insert(x)) == s1.intersection(s2).insert(x))
  }

  property("intersection not-isElem non-empty") = forAll { (s1: Set[Elem], s2: Set[Elem], x: Elem) =>
    !s1.isElem(x) ==> (s1.intersection(s2.insert(x)) == s1.intersection(s2))
  }

  property("difference empty") = forAll { s: Set[Elem] =>
    s.difference(empty) == s
  }

  property("difference non-empty") = forAll { (s1: Set[Elem], s2: Set[Elem], x: Elem) =>
    s1.difference(s2.insert(x)) == s1.difference(s2).delete(x)
  }
}

case class BSTSetTest[A](implicit val arbitraryElem: Arbitrary[A], implicit val ord: Ordering[A]) extends Test {
  override type Elem = A

  override type Set[A] = BSTSet[A]

  override def empty = BSTSet()

  implicit val arbitrarySet = BSTSet.arbitrary
}

object TestBSTSetInt extends SetAxioms(BSTSetTest[Int])

object TestBSTSetChar extends SetAxioms(BSTSetTest[Char])

object TestBSTSetTestData extends SetAxioms(BSTSetTest[TestData.Type])


case class ListSetTest[A](implicit val arbitraryElem: Arbitrary[A]) extends Test {
  override type Elem = A

  override type Set[A] = ListSet[A]

  override def empty = ListSet()

  implicit val arbitrarySet = ListSet.arbitrary
}

object TestListSetInt extends SetAxioms(ListSetTest[Int])

object TestListSetChar extends SetAxioms(ListSetTest[Char])

object TestListSetTestData extends SetAxioms(ListSetTest[TestData.Type])
