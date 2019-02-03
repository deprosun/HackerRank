package com.karangupta.hackerrank.solution

object SherlockandAnagrams {

  def factorial(n: Int): BigInt = {
    def loop(i: Int, answer: BigInt = 1): BigInt = {
      if (i > 0) loop(i - 1, i * answer) else answer
    }

    loop(n)
  }

  def updateKey(key: String, map: Map[String, Int]): Map[String, Int] = map.get(key) map { value =>
    map.updated(key, value + 1)
  } getOrElse {
    map + (key -> 1)
  }

  def sherlockAndAnagrams(s: String): BigInt = {

    def findSubStrings(i: Int, map: Map[String, Int]): Map[String, Int] = {

      def inner(j: Int, map: Map[String, Int]): Map[String, Int] = {
        if (j >= s.length)
          map
        else {

          val word = s.substring(i, j + 1)

          val key = word.sorted

          val newMap = updateKey(key, map)

          inner(j + 1, newMap)

        }

      }

      if (i >= s.length) map
      else {
        val j = i + 1
        findSubStrings(j, inner(j, updateKey(s.charAt(i).toString, map)))
      }

    }

    val initial: BigInt = 0

    val map = findSubStrings(0, Map())

    map.foldLeft(initial) { case (acc, (_, numStrings)) =>
      val n = numStrings
      val fact1 = factorial(n)
      val fact2 = factorial(n - 2)
      acc + {
        fact1 / (2 * fact2)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    println(sherlockAndAnagrams("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  }
}
