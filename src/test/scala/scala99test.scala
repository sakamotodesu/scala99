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

  test("P07 flat") {
    assert(flatten(List(1, 1, 2, 3, 5, 8)) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07 nest right") {
    assert(flatten(List(List(1, List(1, List(2, List(3, List(5, List(8)))))))) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07 nest left") {
    assert(flatten(List(List(List(List(List(List(1), 1), 2), 3), 5), 8)) === List(1, 1, 2, 3, 5, 8))
  }

  test("P07 pair") {
    assert(flatten(List(List(1, 1), List(2, 3), List(5, 8))) === List(1, 1, 2, 3, 5, 8))
  }

  test("P08") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("P08 empty") {
    assert(compress(List()) === List())
  }

  test("P08 one") {
    assert(compress(List('a)) === List('a))
  }

  test("P08 all different") {
    assert(compress(List('a, 'b, 'c, 'd, 'e)) === List('a, 'b, 'c, 'd, 'e))
  }

  test("P09") {
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("P09 empty") {
    assert(pack(List()) === List(List()))
  }

  test("P09 one") {
    assert(pack(List('a)) === List(List('a)))
  }

  test("P09 all different") {
    assert(pack(List('a, 'b, 'c, 'd, 'e)) === List(List('a), List('b), List('c), List('d), List('e)))
  }

  test("P10") {
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("P11") {
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  test("P12") {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("P13") {
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("P14") {
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("P15") {
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("P16") {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17") {
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("P18") {
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g))
  }

  test("P19 +") {
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("P19 -") {
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("P20") {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) ===(List('a, 'c, 'd), 'b))
  }

  test("P21") {
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd))
  }

  test("P22") {
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  test("P23") {
    val ret = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    assert(ret.length === 3)
    assert(ret === ret.distinct)
  }

  test("P24") {
    val ret = lotto(6, 49)
    assert(ret.length === 6)
    assert(!ret.exists(_ < 1))
    assert(ret === ret.distinct)
  }

  test("P25") {
    val ret = randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    assert(ret.length === 6)
    assert(ret === ret.distinct)
    assert(ret !== List('a, 'b, 'c, 'd, 'e, 'f))
  }

}
