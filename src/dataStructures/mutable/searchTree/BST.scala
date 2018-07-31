/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.searchTree

object BST {
  private case class Node[E](var elem: E, var lt: Node[E], var rt: Node[E])

  def empty[A]: BST[A] =
    new BST[A]

  def apply[A](): BST[A] =
    new BST[A]

  private def searchAux[A](node: Node[A], e: A)(implicit ord: Ordering[A]): Option[A] =
    if (node == null)
      None
    else {
      val cmp = ord.compare(e, node.elem)
      if (cmp == 0)
        Some(e)
      else if (cmp < 0)
        searchAux(node.lt, e)
      else
        searchAux(node.rt, e)
    }

  private def split[A](node: Node[A]): (A, Node[A]) =
    if (node.lt == null)
      (node.elem, node.rt)
    else {
      val (x, splitLt) = split(node.lt)
      node.lt = splitLt
      (x, node)
    }

  private def join[A](node: Node[A]): Node[A] =
    if (node.lt == null)
      return node.rt
    else if (node.rt == null)
      return node.lt
    else {
      val (elem, splitRt) = split(node.rt)
      node.elem = elem
      node.rt = splitRt
      return node
    }

  private def deleteMinimAux[A](node: Node[A]): Node[A] =
    if (node.lt == null)
      return node.rt
    else {
      node.lt = deleteMinimAux(node.lt)
      return node
    }

  private def deleteMaximAux[A](node: Node[A]): Node[A] =
    if (node.rt == null)
      return node.lt
    else {
      node.rt = deleteMaximAux(node.rt)
      return node
    }

  private def copyAux[A](node: Node[A]): Node[A] =
    if (node == null)
      null
    else
      Node(node.elem, copyAux(node.lt), copyAux(node.rt))
}

class BST[A](private var root: BST.Node[A], private var sz: Int) extends SearchTree[A] {
  def this() {
    this(null, 0)
  }

  override def isEmpty: Boolean =
    root == null

  override def size: Int =
    sz

  override def search(e: A)(implicit ord: Ordering[A]): Option[A] =
    BST.searchAux(root, e)

  override def isElem(e: A)(implicit ord: Ordering[A]): Boolean =
    search(e) match {
      case None =>
        false
      case Some(_) =>
        true
    }

  private def insertAux(node: BST.Node[A], e: A)(implicit ord: Ordering[A]): BST.Node[A] = {
    var result: BST.Node[A] = node
    if (node == null) {
      result = BST.Node(e, null, null)
      sz += 1
    } else {
      val cmp = ord.compare(e, node.elem)
      if (cmp == 0)
        node.elem = e
      else if (cmp < 0)
        node.lt = insertAux(node.lt, e)
      else
        node.rt = insertAux(node.rt, e)
    }
    return result
  }

  override def insert(e: A)(implicit ord: Ordering[A]): Unit =
    root = insertAux(root, e)

  private def deleteAux(node: BST.Node[A], e: A)(implicit ord: Ordering[A]): BST.Node[A] = {
    var result: BST.Node[A] = node
    if (node == null) {
      ;
    } else {
      val cmp = ord.compare(e, node.elem)
      if (cmp == 0) {
        result = BST.join(node)
        sz -= 1
      } else if (cmp < 0)
        node.lt = deleteAux(node.lt, e)
      else
        node.rt = deleteAux(node.rt, e)
    }
    return result
  }

  override def delete(e: A)(implicit ord: Ordering[A]): Unit =
    root = deleteAux(root, e)

  override def minim: A =
    if (root == null)
      throw EmptySearchTreeException("minim on empty tree")
    else {
      var node = root
      while (node.lt != null)
        node = node.lt
      return node.elem
    }

  override def maxim: A =
    if (root == null)
      throw EmptySearchTreeException("maxim on empty tree")
    else {
      var node = root
      while (node.rt != null)
        node = node.rt
      return node.elem
    }

  override def deleteMinim: Unit =
    if (root == null)
      throw EmptySearchTreeException("deleteMinim on empty tree")
    else {
      root = BST.deleteMinimAux(root)
      sz -= 1
    }

  override def deleteMaxim: Unit =
    if (root == null)
      throw EmptySearchTreeException("deleteMaxim on empty tree")
    else {
      root = BST.deleteMaximAux(root)
      sz -= 1
    }

  override def copy: BST[A] =
    new BST[A](BST.copyAux(root), sz)

  // An iterator on elements in tree
  private trait Traversal extends Iterator[A] {
    val stack = dataStructures.mutable.stack.ArrayStack[Either[A, BST.Node[A]]]

    def save(node: BST.Node[A]): Unit

    if (root != null)
      save(root)

    def hasNext: Boolean =
      !stack.isEmpty

    def next: A =
      if (!hasNext)
        throw new NoSuchElementException
      else {
        var either = stack.top
        stack.pop()
        while (either.isRight) {
          val node = either.right.get
          save(node)
          either = stack.top
          stack.pop()
        }
        return either.left.get
      }
  }

  override def inOrderIt: Iterator[A] = new Traversal {
    def save(node: BST.Node[A]): Unit = { // in reverse order, cause stack is LIFO
      if (node.rt != null)
        stack.push(Right(node.rt))

      stack.push(Left(node.elem))

      if (node.lt != null)
        stack.push(Right(node.lt))
    }
  }

  override def inOrder: Iterable[A] = new Iterable[A] {
    override def iterator =
      inOrderIt
  }

  override def preOrderIt: Iterator[A] = new Traversal {
    def save(node: BST.Node[A]): Unit = { // in reverse order, cause stack is LIFO
      if (node.rt != null)
        stack.push(Right(node.rt))

      if (node.lt != null)
        stack.push(Right(node.lt))

      stack.push(Left(node.elem))
    }
  }

  override def preOrder: Iterable[A] = new Iterable[A] {
    override def iterator =
      preOrderIt
  }

  override def postOrderIt: Iterator[A] = new Traversal {
    def save(node: BST.Node[A]): Unit = { // in reverse order, cause stack is LIFO
      stack.push(Left(node.elem))

      if (node.rt != null)
        stack.push(Right(node.rt))

      if (node.lt != null)
        stack.push(Right(node.lt))
    }
  }

  override def postOrder: Iterable[A] = new Iterable[A] {
    override def iterator =
      postOrderIt
  }

  override def toString: String = {
    def aux[A](node: BST.Node[A]): String =
      if (node == null)
        "null"
      else
        s"Node(${node.elem}, ${aux(node.lt)}, ${aux(node.rt)})"

    return aux(root)
  }
}
