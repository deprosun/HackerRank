package com.karangupta.hackerrank.solution

object CountTriplets {

  def updateKey(key: Long, map: Map[Long, Int]): Map[Long, Int] = map.get(key) map { value =>
    map.updated(key, value + 1)
  } getOrElse {
    map + (key -> 1)
  }

  def countTriplets(arr: Array[Long], r: Long): Long = {

    def getMap(array: Seq[Long], map: Map[Long, Int], max: Long): (Map[Long, Int], Long) = {
      array match {
        case Seq() => (map, max)
        case Seq(head, tail@_*) =>
          val newMap = updateKey(head, map)
          getMap(tail, newMap, if (max > head) max else head)
      }
    }

    val (map, max) = getMap(arr.sorted, Map(), 0L)

    map.foldLeft(0L) { case (acc, (a, aValue)) =>
      val b = a * r

      val bValue = if (b > max) 0 else if (b == a) map.getOrElse(b, 0) - 1 else map.getOrElse(b, 0)

      val cValue = if (bValue == 0) 0 else {
        val c = b * r
        if (c == a) map.getOrElse(c, 0) - 2
        else if (c == b) map.getOrElse(c, 0) - 1 else map.getOrElse(c, 0)
      }

      acc + (aValue * bValue * cValue)
    }

  }

  def countTriplets2(arr: Array[Long], r: Long): Long = {



    null

  }

  def main(args: Array[String]): Unit = {
    val test = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
    val arr = test.split(" ").map(_.toLong)
    val r = 1L

    println(countTriplets(arr, r))
  }

}
