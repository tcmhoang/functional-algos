package funalgo

import scala.collection.immutable.LazyList.cons

class MergeSortSuite extends munit.FunSuite:
  import MergeSort.*

  test("Sort a empty list")(
    assertEquals(sort(List.empty[Int]), Nil)
  )

//   test("Sort a list with one elements")(
//     assertEquals(sort(cons(1, Nil)), cons(1, Nil))
//   )

//   test("Check if the list is sorted with arbitrary List")
