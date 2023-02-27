package funalgo

import scala.collection.immutable.HashMap

class HashTableSuite extends munit.FunSuite {
  test("Hashtable working properly")(() =>
    val table = TrieHashTable
      .intString(10)
      .insert(123, "Sam")
      .insert(312, "Allison")
      .insert(213, "Todd")

    assertEquals(table.search(123), Some("Sam"))
    assertEquals(table.search(213), Some("Todd"))
    assertEquals(table.search(333), None)

    val removed = table.delete(123)
    assertEquals(removed.search(123), None)
    assertEquals(table.search(213), Some("Todd"))
  )
}
