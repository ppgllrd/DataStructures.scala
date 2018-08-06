/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

trait QueueFactory[A] {
  def empty: Queue[A]
}
