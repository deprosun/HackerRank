import java.io._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


object FrequencyQueries {

  def incrementNumberCount(number: Int, numberCount: Map[Int, Int]): Map[Int, Int] = {
    numberCount.get(number) map { current =>
      numberCount + (number -> (current + 1))
    } getOrElse {
      numberCount + (number -> 1)
    }
  }

  def incrementFrequencyCount(count: Int, frequencyCount: Map[Int, Int]): Map[Int, Int] = {
    if (count == 1) {
      frequencyCount.get(1) map { numberCount =>
        frequencyCount + (1 -> (numberCount + 1))
      } getOrElse {
        frequencyCount + (1 -> 1)
      }
    } else {
      val reducedFrequency = count - 1
      val reduced = frequencyCount.get(reducedFrequency) map {
        case 1 => frequencyCount - reducedFrequency
        case nc => frequencyCount + (reducedFrequency -> (nc - 1))
      } getOrElse {
        frequencyCount
      }

      reduced.get(count) map { nc =>
        reduced + (count -> (nc + 1))
      } getOrElse {
        reduced + (count -> 1)
      }
    }
  }

  def decrementNumberCount(number: Int, numberCount: Map[Int, Int]): Map[Int, Int] = {
    numberCount.get(number) map {
      case 1 => numberCount - number
      case count => numberCount + (number -> (count - 1))
    } getOrElse {
      numberCount
    }
  }

  def decrementFrequencyCount(count: Int, frequencyCount: Map[Int, Int]): Map[Int, Int] = {

    if (count == 1) {
      frequencyCount - 1
    } else {
      frequencyCount.get(count) map {
        case 1 => frequencyCount - count
        case nc =>
          val reduced = frequencyCount + (count -> (nc - 1))
          val incrementKey = count - 1
          reduced.get(incrementKey) map { nc =>
            reduced + (incrementKey -> (nc + 1))
          } getOrElse {
            reduced + (incrementKey -> 1)
          }
      } getOrElse {
        frequencyCount
      }
    }

  }

  // Complete the freqQuery function below.
  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {

    val numberCount = Map[Int, Int]()
    val frequencyCount = Map[Int, Int]()
    val array = ArrayBuffer[Int]()

    queries.foldLeft((numberCount, frequencyCount)) {
      case (acc, Array(1, n)) =>
        val (nCount, fCount) = acc

        val newNumberCount = incrementNumberCount(n, nCount)
        val frequency = newNumberCount(n)
        val newFrequencyCount = incrementFrequencyCount(frequency, fCount)

        (newNumberCount, newFrequencyCount)
      case (acc, Array(2, n)) =>
        val (nCount, fCount) = acc

        val newNumberCount = decrementNumberCount(n, nCount)
        val newFrequencyCount = nCount.get(n) map { count =>
          decrementFrequencyCount(count, fCount)
        } getOrElse {
          fCount
        }

        (newNumberCount, newFrequencyCount)
      case (acc, Array(3, n)) =>
        val (nCount, fCount) = acc

        val zeroOrOne = if (n == 0) 1 else {
          fCount.get(n) map { _ => 1 } getOrElse 0
        }

        array += zeroOrOne

        (nCount, fCount)

    }

    array.toArray
  }

  def main(args: Array[String]) {

    val fileReader = new FileReader("/Users/kgupta/HackerRank/src/main/resources/FrequencyQueries_Input1.txt")
    val reader = new BufferedReader(fileReader)

    val q = reader.readLine().toInt

    val queries = Array.ofDim[Int](q, 2)

    def read(i: Int, line: String): Unit = {
      if (line == null) {
        reader.close()
        fileReader.close()
      }
      else {
        queries(i) = line.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
        read(i + 1, reader.readLine())
      }
    }

    read(0, reader.readLine())

    freqQuery(queries) foreach println
  }
}
