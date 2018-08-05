/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.searchTree

import org.scalacheck.{Arbitrary, Gen}

object BST {
  def empty[A](implicit ord: Ordering[A]): SearchTree[A] =
    factory[A](ord).empty

  def apply[A]()(implicit ord: Ordering[A]): SearchTree[A] =
    factory[A](ord).empty

  def factory[A](implicit ord: Ordering[A]): SearchTreeFactory[A] =
    new BSTFactory[A]()(ord)

  implicit def arbitrary[A](implicit a: Arbitrary[A], ord: Ordering[A]) = Arbitrary[SearchTree[A]] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(empty[A](ord))((x, bst) => bst.insert(x))
  }
}
