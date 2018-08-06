/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.stack

import scala.reflect.ClassTag

class ArrayStack[A: ClassTag](initialCapacity: Int) extends Stack[A] {
  private var elements = new Array[A](initialCapacity)
  private var nextFree = 0

  def this() {
    this(ArrayStack.defaultInitialCapacity)
  }

  private def ensureCapacity(): Unit =
    if (nextFree >= elements.length) {
      val newArray = new Array[A](2 * elements.length)
      elements.copyToArray(newArray)
      elements = newArray
    }

  override def isEmpty: Boolean =
    nextFree == 0

  override def push(x: A): Unit = {
    ensureCapacity()
    elements(nextFree) = x
    nextFree += 1
  }

  override def pop(): Unit =
    if (isEmpty)
      throw EmptyStackException("pop on empty stack")
    else
      nextFree -= 1

  override def top: A =
    if (isEmpty)
      throw EmptyStackException("top on empty stack")
    else
      elements(nextFree - 1)

  override def toString: String = {
    val sb = new StringBuilder(this.getClass.getSimpleName)
    sb.append('(')
    for (i <- nextFree - 1 to 1 by -1) {
      sb.append(elements(i))
      sb.append(',')
    }
    if (!isEmpty)
      sb.append(elements(0))
    sb.append(')')
    sb.toString
  }
}

case class ArrayStackFactory[A: ClassTag](initialCapacity: Int) extends StackFactory[A] {
  def this() {
    this(ArrayStack.defaultInitialCapacity)
  }

  def empty: ArrayStack[A] =
    new ArrayStack[A](initialCapacity)
}

object ArrayStack {
  val defaultInitialCapacity = 128

  def empty[A: ClassTag](): ArrayStack[A] =
    new ArrayStack[A]()

  def empty[A: ClassTag](initialCapacity: Int): ArrayStack[A] =
    new ArrayStack[A](initialCapacity)

  def apply[A: ClassTag](): ArrayStack[A] =
    new ArrayStack[A]()

  def apply[A: ClassTag](initialCapacity: Int): ArrayStack[A] =
    new ArrayStack[A](initialCapacity)

  def factory[A: ClassTag]: ArrayStackFactory[A] =
    new ArrayStackFactory[A]()

  def factory[A: ClassTag](initialCapacity: Int): ArrayStackFactory[A] =
    new ArrayStackFactory[A](initialCapacity)
}