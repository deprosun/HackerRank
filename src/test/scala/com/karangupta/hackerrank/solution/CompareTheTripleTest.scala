package com.karangupta.hackerrank.solution

import CompareTheTriple._

class CompareTheTripleTest extends UnitSpec {

  behavior of "A non-empty two dimensional list"

  it should "yield a list of length that equals length of the two dimensional list" in {
    val table = List(
      List(1, 2, 3),
      List(3, 4, 5)
    )

    val actual = compute(table).length
    val expected = 2

    assert(actual == expected, "Incorrect answer.")
  }

  it should "yield the correct answer" in {
    val table = List(
      List(1, 2, 3),
      List(3, 4, 5)
    )

    val actual = compute(table)
    val expected = List(0, 3)

    assert(actual == expected, "Incorrect answer.")
  }

  it should "yield the correct answer when there are three players" in {
    val table = List(
      List(1, 2, 3),
      List(3, 4, 5),
      List(6, 7, 8)
    )

    val actual = compute(table)
    val expected = List(0, 0, 3)

    assert(actual == expected, "Incorrect answer.")
  }

  it should "yield the correct answer when there exists two players that have the same score for the same index (category)" in {
    val table = List(
      List(1, 2, 3),
      List(6, 4, 5),
      List(6, 7, 8)
    )

    val actual = compute(table)
    val expected = List(0, 0, 2)

    assert(actual == expected, "Incorrect answer.")
  }


}
