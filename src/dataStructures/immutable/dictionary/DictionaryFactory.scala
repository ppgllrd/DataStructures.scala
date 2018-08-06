/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.dictionary

trait DictionaryFactory[K, V] {
  def empty: Dictionary[K, V]
}
