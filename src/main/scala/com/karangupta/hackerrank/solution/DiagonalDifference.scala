package com.karangupta.hackerrank.solution

import scala.language.postfixOps

object DiagonalDifference {

  def compute(table: Array[Array[Int]]): Int = {

    val length = table.length

    val leftToRight = table.foldLeft((0, 0)) { (acc, x) =>
      val (sum, col) = acc
      if (col == length)
        acc
      else
        (sum + x(col), col + 1)
    } _1

    val rightToLeft = table.foldLeft((0, length - 1)) { (acc, x) =>
      val (sum, col) = acc
      if (col < 0)
        acc
      else
        (sum + x(col), col - 1)
    } _1

    Math.abs(leftToRight - rightToLeft)
  }

}
