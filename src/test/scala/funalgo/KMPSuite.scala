package funalgo

import org.scalacheck.*
import Gen.*
import Arbitrary.*
import Prop.{forAll}

import java.util.concurrent.ThreadLocalRandom

class KMPSuite extends munit.FunSuite:
  import KMP.*

  test("If the pattern empty return prefix list with 0 in it")(
    assertEquals(buildPrefixs(""), Vector(0))
  )

  test("Build the right prefix list for non empty pattern")(
    assertEquals(buildPrefixs("odoodledo"), Vector(0, 0, 1, 1, 2, 0, 0, 0, 1))
  )

  test("Find the right position in the string")(
    assertEquals(find("dododydodoodoodledo", "odoodledo"), Some(10))
  )

  test("If not found substring return None")(
    assertEquals(find("dododydodoodoodledo", "odoodedo"), None)
  )

  lazy val genPairs =
    for {
      n <- arbitrary[String]
      m <- oneOf(
        n.substring(ThreadLocalRandom.current().nextInt(0, n.length())),
        arbString
      )
    } yield (n, m)

  test("Find the substring like built-in function") {
    forAll { (s: String, sb: String) =>
      find(s, sb).getOrElse(-1) == s.indexOf(sb)
    }.check()
  }
