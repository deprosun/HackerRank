package com.karangupta.hackerrank.solution

import java.util
import java.util.Collections

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

object CountTriplets {

  def toBST(array: Array[Int]): Tree = {
    def loop(start: Int, end: Int): Tree = {
      if (start > end) return EmptyNode
      val mid = (start + end) / 2
      val left = loop(start, mid - 1)
      val right = loop(mid + 1, end)
      DataNode(array(mid), left, right)
    }

    loop(0, array.length - 1)
  }

  def bstToString(tree: Tree): String = {
    tree match {
      case EmptyNode => ""
      case DataNode(d, EmptyNode, EmptyNode) => d.toString
      case DataNode(d, EmptyNode, right: DataNode) => d + bstToString(right)
      case DataNode(d, left: DataNode, EmptyNode) => bstToString(left) + d
      case DataNode(d, left, right) => bstToString(left) + d + bstToString(right)
    }
  }

  //  def updateKey(key: Long, map: Map[Long, Int]): Map[Long, Int] = map.get(key) map { value =>
  //    map.updated(key, value + 1)
  //  } getOrElse {
  //    map + (key -> 1)
  //  }
  //
  //  def countTriplets(arr: Array[Long], r: Long): Long = {
  //
  //    def getMap(array: Seq[Long], map: Map[Long, Int], max: Long): (Map[Long, Int], Long) = {
  //      array match {
  //        case Seq() => (map, max)
  //        case Seq(head, tail@_*) =>
  //          val newMap = updateKey(head, map)
  //          getMap(tail, newMap, if (max > head) max else head)
  //      }
  //    }
  //
  //    val (map, max) = getMap(arr.sorted, Map(), 0L)
  //
  //    map.foldLeft(0L) { case (acc, (a, aValue)) =>
  //      val b = a * r
  //
  //      val bValue = if (b > max) 0 else if (b == a) map.getOrElse(b, 0) - 1 else map.getOrElse(b, 0)
  //
  //      val cValue = if (bValue == 0) 0 else {
  //        val c = b * r
  //        if (c == a) map.getOrElse(c, 0) - 2
  //        else if (c == b) map.getOrElse(c, 0) - 1 else map.getOrElse(c, 0)
  //      }
  //
  //      acc + (aValue * bValue * cValue)
  //    }
  //
  //  }
  //
  def countTriplets2(arr: Array[Long], r: Long): Long = {

    def indicesMap(i: Int, indices: Map[Long, util.List[Integer]]): Map[Long, util.List[Integer]] = {
      if (i >= arr.length) return indices

      indices.get(arr(i)) match {
        case None =>
          val newList = new util.ArrayList[Integer]()
          newList.add(i)
          val map = indices + (arr(i) -> newList)
          indicesMap(i + 1, map)
        case Some(list) =>
          list.add(i)
          val map = indices + (arr(i) -> list)
          indicesMap(i + 1, map)
      }
    }

    val indexMap = indicesMap(0, Map())

    def loop(i: Int, result: Long): Long = {

      if (i >= arr.length) return result

      if (arr(i) % r != 0) loop(i + 1, result)
      else {

        val firstNumber = arr(i) / r

        if (!indexMap.contains(firstNumber)) return loop(i + 1, result)

        val lastNumber = arr(i) * r

        if (!indexMap.contains(lastNumber)) return loop(i + 1, result)

        val countBefore: Long = {
          val indices = indexMap(firstNumber)
          val p = Collections.binarySearch(indices, new Integer(i))
          if (p < 0) -1 - p else p
        }

        val countAfter: Long = {

          val indices = indexMap(lastNumber)

          val p = Collections.binarySearch(indices, new Integer(i))

          val p2 = if (p < 0) -1 - p - 1 else p

          indices.size() - 1 - p2
        }

        val count = countBefore * countAfter

        loop(i + 1, result + count)

      }
    }

    loop(0, 0)


  }

  def main(args: Array[String]): Unit = {
    val test = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
    val arr = test.split(" ").map(_.toLong)
    val r = 1L

    //    println(countTriplets2(Array(1, 2, 1, 2, 4), 2))

    //    println(toBST(Array(1, 2, 3, 4, 5)))

    println(bstToString(toBST(Array(1, 2, 3, 4, 5))))

  }

}

trait Tree

case object EmptyNode extends Tree

case class DataNode(d: Int, left: Tree, right: Tree) extends Tree

