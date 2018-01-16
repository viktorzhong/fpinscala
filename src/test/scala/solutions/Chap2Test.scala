package solutions

import org.scalatest.FunSuite
import solutions.Chap2._

class Chap2Test extends FunSuite {

  test("testFind") {
    val arr = Array("abc", "def")
    assert(find[String](arr, (x) => x == "def") === 1)
  }

  test("isSorted test") {
    val list = Array(1, 2, 3)
    assert(isSorted[Int](list, (x, y) => x > y ) === true)
  }

  test("isSorted test, return false") {
    val list = Array(1, 5, 3)
    assert(isSorted[Int](list, (x, y) => x > y ) === false)
  }
}
