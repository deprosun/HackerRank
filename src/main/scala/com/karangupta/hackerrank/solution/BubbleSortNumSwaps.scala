package com.karangupta.hackerrank.solution

object BubbleSortNumSwaps {

  def swap(array: Array[Int], i: Int, j: Int): Unit = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

  def bubbleSort(array: Array[Int]): Unit = {

    def outerLoop(i: Int, swaps: Int, limit: Int): Int = {

      def innerLoop(j: Int, swaps: Int, limit: Int): Int = {
        if (j >= limit) return swaps

        if (array(j) > array(j + 1)) {
          swap(array, j, j + 1)
          innerLoop(j + 1, swaps + 1, limit)
        } else {
          innerLoop(j + 1, swaps, limit)
        }

      }

      if (i >= array.length) swaps
      else {
        val newSwaps = innerLoop(0, swaps, limit)
        outerLoop(i + 1, newSwaps, limit - 1)
      }

    }

    val (arr, s) = (array, outerLoop(0, 0, array.length - 1))

    println(s)
    println(arr(0))
    println(arr(arr.length - 1))

  }

  def main(args: Array[String]): Unit = {

  }

}
