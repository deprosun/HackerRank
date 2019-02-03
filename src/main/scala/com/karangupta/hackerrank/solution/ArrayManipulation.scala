package com.karangupta.hackerrank.solution

import scala.io.Source
import scala.language.postfixOps
import scala.collection.Seq

object ArrayManipulation {

  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {
    val arr = Array.fill[Int](n)(0)

    queries foreach { case Array(a, b, k) =>
      arr(a - 1) = arr(a - 1) + k
      if (b < n) arr(b) = arr(b) - k
    }

    val max = 0L
    val sum = 0L

    val (result, _) = arr.foldLeft((max, sum)) { case ((currentMax, currentSum), elem) =>
      val newSum = currentSum + elem
      if (newSum > currentMax) (newSum, newSum) else (currentMax, newSum)
    }

    result
  }

  def foo(list: Seq[Int]) = {
    list match {
      case Seq() =>
      case Seq(s, tail@_*) =>
    }
  }


  def main(args: Array[String]): Unit = {

    val reader = Source.fromFile("/Users/kgupta/Downloads/input04.txt").bufferedReader()

    //    val initial: (Array[(Int, Int, Long)], Long) = (Array((1, 4000, 0L)), 0L)

    val n = 4000

    val arr = 0 to (n + 1) map (_ => 0) toArray

    def process(line: String): Unit = {
      if (line == null) {}
      else {
        val Array(a, b, k) = line.split(" ").map(_.trim.toInt)

        arr(a) = arr(a) + k
        if (b + 1 <= n) arr(b + 1) = arr(b + 1) - k

        //        val result = arrayManipulation(acc, query)
        process(reader.readLine())
      }
    }

    //    val result = arrayManipulation(5, Array(Array(1, 2, 100), Array(2, 5, 100), Array(3, 4, 100)))

    reader.readLine()

    process(reader.readLine())

    val tempMax = 0L

    val finalMax = (0 to n).foldLeft(tempMax) { (acc, x) =>
      if (arr(x) > acc) arr(x) else acc
    }

    println(finalMax)
  }

}
