import org.scalatest.FunSuite

import scala99._

class scala99test extends FunSuite {

  test("P01") {
    assert(last(List(1, 1, 2, 3, 5, 8)) === 8)
  }

  test("P02") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
  }

  test("P03") {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  test("P04") {
    assert(length(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  test("P05") {
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  test("P06") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("P06-2") {
    assert(!isPalindrome(List(1, 2, 3, 2, 4)))
  }

  test("P07") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07-2") {
    assert(flatten(List(1, 1, 2, 3, 5, 8)) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07-3") {
    assert(flatten(List(List(1, List(1, List(2, List(3, List(5, List(8)))))))) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07-4") {
    assert(flatten(List(List(List(List(List(List(1), 1), 2), 3), 5), 8)) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07-5") {
    assert(flatten(List(List(1, 1), List(2, 3), List(5, 8))) === List(1, 1, 2, 3, 5, 8))
  }
}
