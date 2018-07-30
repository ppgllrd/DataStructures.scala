/******************************************************************************
 * Data Structures in Scala
 *
 * Pepe Gallardo, 2018
 *****************************************************************************/

package dataStructures.immutable.searchTree

sealed trait BST[+A] extends SearchTree[A] {
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

  override def search[B >: A](e: B)(implicit ord: Ordering[B]): Option[A] = this match {
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

  override def isElem[B >: A](e: B)(implicit ord: Ordering[B]): Boolean = search(e) match {
    case None =>
      false
    case Some(_) =>
      true
  }

  override def insert[B >: A](e: B)(implicit ord: Ordering[B]): BST[B] = this match {
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

  override def delete[B >: A](e: B)(implicit ord: Ordering[B]): BST[A] = this match {
    case Empty =>
      Empty
    case Node(x, lt, rt) =>
      val cmp = ord.compare(e, x)
      if (cmp == 0)
        BST.join(lt, rt)
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

  override def deleteMinim: BST[A] = this match {
    case Empty =>
      throw EmptySearchTreeException("deleteMinim on empty tree")
    case Node(x, Empty, rt) =>
      rt
    case Node(x, lt, rt) =>
      Node(x, lt.deleteMinim, rt)
  }

  override def deleteMaxim: BST[A] = this match {
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
  private def traversal[B](z:B)(f : (A, B) => B)(order : (B => B, B => B, B => B) => B => B): B = {
    def aux(bst : BST[A]) : B => B = bst match {
      case Empty =>
        identity
      case Node(x, lt, rt) =>
        order(f(x,_), aux(lt), aux(rt))
    }
    aux(this)(z)
  }

  def foldPreOrder2[B](z : B)(f : (A, B) => B) : B =
    traversal(z)(f)((xf, lf, rf) => xf compose lf compose rf)
  def foldInOrder2[B](z : B)(f : (A, B) => B) : B =
    traversal(z)(f)((xf, lf, rf) => lf compose xf compose rf)
  def foldPostOrder2[B](z : B)(f : (A, B) => B) : B =
    traversal(z)(f)((xf, lf, rf) => lf compose rf compose xf)
}

private case object Empty extends BST[Nothing]

private case class Node[A](x : A, lt : BST[A], rt : BST[A]) extends BST[A] {
  def split : (A, BST[A]) = this match {
    case Node(x, Empty, rt) =>
      (x, rt)
    case Node(x, lt@Node(_, _, _), rt) =>
      val (y, lt2) = lt.split
      (y, Node(x, lt2, rt))
  }
}

object BST {
  def empty[A] : BST[A] =
    Empty

  def apply[A](): BST[A] =
    Empty

  def apply[A](xs : A*)(implicit ord : Ordering[A]) : BST[A] =
    xs.foldLeft(empty[A])(_.insert(_))

  private def join[A](lt : BST[A], rt : BST[A]) : BST[A] =
    (lt, rt) match {
      case (Empty, _) =>
        rt
      case (_, Empty) =>
        lt
      case (_, rt@Node(_, _, _)) =>
        val (x, splittedRt) = rt.split
        Node(x, lt, splittedRt)
    }
}

