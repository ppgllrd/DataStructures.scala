/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.searchTree

trait SearchTreeFactory[A] {
  def empty: SearchTree[A]
}
