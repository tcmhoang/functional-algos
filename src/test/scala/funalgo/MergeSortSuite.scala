package funalgo

import scala.collection.immutable.LazyList.cons
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Arbitrary

class MergeSortSuite extends munit.FunSuite:
  import MergeSort.*

  test("Sort a empty list")(
    assertEquals(sort(List.empty[Int]), Nil)
  )

  test("Sort a list with one elements")(
    assertEquals(sort(cons(1, LazyList.empty[Int])), 1 :: Nil)
  )

  test("sorted like built-in function") {
    forAll { (ls: List[Int]) =>
      val sorted = sort(ls)
      val bsorted = ls.sorted
      sorted == bsorted
    }.check()
  }
