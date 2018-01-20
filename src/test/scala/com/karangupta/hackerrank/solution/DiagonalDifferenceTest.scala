package com.karangupta.hackerrank.solution

import DiagonalDifference._

class DiagonalDifferenceTest extends UnitSpec {

  behavior of "Non empty N x N matrix"

  it should "yield correct answer" in {
    val matrix = Array(
      Array(1, 2, 3),
      Array(1, 2, 3),
      Array(1, 2, 3)
    )

    val actual = compute(matrix)
    val expected = 0

    assert(actual == expected, "Incorrect answer.")
  }

}
