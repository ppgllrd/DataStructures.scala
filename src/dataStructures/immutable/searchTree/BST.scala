package dataStructures.immutable.searchTree

import org.scalacheck.{Arbitrary, Gen}

case class BSTFactory[A](implicit ord: Ordering[A]) extends SearchTreeFactory[A] {
  private def join(lt: Tree, rt: Tree): Tree =
    (lt, rt) match {
      case (Empty, _) =>
        rt
      case (_, Empty) =>
        lt
      case (_, rt@Node(_, _, _)) =>
        val (x, splitRt) = rt.split
        Node(x, lt, splitRt)
    }

  sealed trait Tree extends SearchTree[A] {
    override def isEmpty: Boolean = this match {
      case Empty =>
        true
      case Node(_, _, _) =>
        false
    }

    override def size: Int = this match {
      case Empty =>
        0
      case Node(_, lt, rt) =>
        1 + lt.size + rt.size
    }

    override def search(e: A): Option[A] = this match {
      case Empty =>
        None
      case Node(x, lt, rt) =>
        val cmp = ord.compare(e, x)
        if (cmp == 0)
          Some(x)
        else if (cmp < 0)
          lt.search(e)
        else
          rt.search(e)
    }

    override def isElem(e: A): Boolean = search(e) match {
      case None =>
        false
      case Some(_) =>
        true
    }

    override def insert(e: A): Tree = this match {
      case Empty =>
        Node(e, Empty, Empty)
      case Node(x, lt, rt) =>
        val cmp = ord.compare(e, x)
        if (cmp == 0)
          Node(e, lt, rt)
        else if (cmp < 0)
          Node(x, lt.insert(e), rt)
        else
          Node(x, lt, rt.insert(e))
    }

    override def delete(e: A): Tree = this match {
      case Empty =>
        Empty
      case Node(x, lt, rt) =>
        val cmp = ord.compare(e, x)
        if (cmp == 0)
          join(lt, rt)
        else if (cmp < 0)
          Node(x, lt.delete(e), rt)
        else
          Node(x, lt, rt.delete(e))
    }

    override def minim: A = this match {
      case Empty =>
        throw EmptySearchTreeException("minim on empty tree")
      case Node(x, Empty, _) =>
        x
      case Node(_, lt, _) =>
        lt.minim
    }

    override def maxim: A = this match {
      case Empty =>
        throw EmptySearchTreeException("maxim on empty tree")
      case Node(x, _, Empty) =>
        x
      case Node(_, _, rt) =>
        rt.maxim
    }

    override def deleteMinim: Tree = this match {
      case Empty =>
        throw EmptySearchTreeException("deleteMinim on empty tree")
      case Node(x, Empty, rt) =>
        rt
      case Node(x, lt, rt) =>
        Node(x, lt.deleteMinim, rt)
    }

    override def deleteMaxim: Tree = this match {
      case Empty =>
        throw EmptySearchTreeException("deleteMaxim on empty tree")
      case Node(x, lt, Empty) =>
        lt
      case Node(x, lt, rt) =>
        Node(x, lt, rt.deleteMaxim)
    }

    override def foldInOrder[B](z: B)(f: (A, B) => B): B = this match {
      case Empty =>
        z
      case Node(x, lt, rt) =>
        lt.foldInOrder(f(x, rt.foldInOrder(z)(f)))(f)
    }

    override def foldPreOrder[B](z: B)(f: (A, B) => B): B = this match {
      case Empty =>
        z
      case Node(x, lt, rt) =>
        f(x, lt.foldPreOrder(rt.foldPreOrder(z)(f))(f))
    }

    override def foldPostOrder[B](z: B)(f: (A, B) => B): B = this match {
      case Empty =>
        z
      case Node(x, lt, rt) =>
        lt.foldPostOrder(rt.foldPostOrder(f(x, z))(f))(f)
    }

    // Alternative implementation of folds using a generic traversal
    private def traversal[B](z: B)(f: (A, B) => B)(order: (B => B, B => B, B => B) => B => B): B = {
      def aux(bst: Tree): B => B = bst match {
        case Empty =>
          identity
        case Node(x, lt, rt) =>
          order(f(x, _), aux(lt), aux(rt))
      }

      aux(this)(z)
    }

    def foldPreOrder2[B](z: B)(f: (A, B) => B): B =
      traversal(z)(f)((xf, lf, rf) => xf compose lf compose rf)

    def foldInOrder2[B](z: B)(f: (A, B) => B): B =
      traversal(z)(f)((xf, lf, rf) => lf compose xf compose rf)

    def foldPostOrder2[B](z: B)(f: (A, B) => B): B =
      traversal(z)(f)((xf, lf, rf) => lf compose rf compose xf)

    override def toString: String = {
      val sb = new StringBuilder

      def aux(bst: Tree): Unit = bst match {
        case Empty =>
          sb.append("Empty")
        case Node(x, lt, rt) =>
          sb.append("Node(")
          sb.append(x)
          sb.append(", ")
          aux(lt)
          sb.append(", ")
          aux(rt)
          sb.append(')')
      }

      aux(this)
      sb.toString
    }
  }

  object Empty extends Tree

  protected case class Node(e: A, lt: Tree, rt: Tree) extends Tree {
    def split: (A, Tree) = this match {
      case Node(x, Empty, rt) =>
        (x, rt)
      case Node(x, lt@Node(_, _, _), rt) =>
        val (y, lt2) = lt.split
        (y, Node(x, lt2, rt))
    }
  }

  override def empty: Tree =
    Empty
}

object BST {
  def empty[A](implicit ord: Ordering[A]): BSTFactory[A]#Tree =
    factory[A].empty

  def apply[A]()(implicit ord: Ordering[A]): BSTFactory[A]#Tree =
    factory[A].empty

  def factory[A](implicit ord: Ordering[A]): BSTFactory[A] =
    new BSTFactory[A]()

  implicit def arbitrary[A](implicit a: Arbitrary[A], ord: Ordering[A]) = Arbitrary[BSTFactory[A]#Tree] {
    for {xs <- Gen.listOf(a.arbitrary)}
      yield xs.foldRight(empty[A](ord))((x, bst) => bst.insert(x))
  }
}