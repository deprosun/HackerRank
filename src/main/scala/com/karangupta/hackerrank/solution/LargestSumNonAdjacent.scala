package com.karangupta.hackerrank.solution

object LargestSumNonAdjacent {

  def newIncl(lastIncl: Int, lastExcl: Int, element: Int): Int = {
    Math.max(lastIncl, element + lastExcl)
  }

  def newExcl(lastIncl: Int, lastExcl: Int): Int = {
    Math.max(lastExcl, lastIncl)
  }

  def largestAdjacentSum(arr: Array[Int]): Int = {

    def loop(i: Int, incl: Int, excl: Int): Int = {

      //return the max between incl and excl
      if (i == arr.length) return Math.max(incl, excl)

      val nIncl = newIncl(incl, excl, arr(i))

      val nExcl = newExcl(incl, excl)

      loop(i + 1, nIncl, nExcl)

    }

    loop(1, arr(0), 0)

  }

  def main(args: Array[String]): Unit = {

    println(largestAdjacentSum(Array(2, 4, 6, 2, 5)))

    println(largestAdjacentSum(Array(5, 1, 1, 5)))

  }


}
