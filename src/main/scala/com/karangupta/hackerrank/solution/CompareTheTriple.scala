package com.karangupta.hackerrank.solution

import scala.language.postfixOps

object CompareTheTriple {

  def compute(table: List[List[Int]]): List[Int] = {
    val players = table.indices
    val categories = table.head.indices

    val initialPoints = players map (_ => 0) toList

    categories.foldLeft(initialPoints) { (acc, c) =>

      val (ps, _) = players.foldLeft((List[Int](), -1)) { (winner, p) =>

        val (ps, winningScore) = winner

        val score = table(p)(c)

        if (score > winningScore) (p :: Nil, score)
        else if (score == winningScore) (ps :+ p, score)
        else winner
      }

      if (ps.length == 1) {

        //get the winning player
        val p = ps.head

        //get the current score of this player
        val currentScore = acc(p)

        //give this player one more point
        val newScore = currentScore + 1

        //return back the array where this player's score is incremented by 1
        acc.updated(p, newScore)
      } else acc

    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)

    //alice
    var a0 = sc.nextInt()
    var a1 = sc.nextInt()
    var a2 = sc.nextInt()

    //bob
    var b0 = sc.nextInt()
    var b1 = sc.nextInt()
    var b2 = sc.nextInt()

    val table: List[List[Int]] =
      List(
        List(a0, a1, a2),
        List(b0, b1, b2)
      )

    println(compute(table).mkString(" "))


  }
}
