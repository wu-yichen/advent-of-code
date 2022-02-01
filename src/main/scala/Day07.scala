import Utils.readFrom

import scala.annotation.tailrec

object Day07 extends App {

  val inputLines =
    readFrom("Day07/Day07-test.txt").flatMap(_.split(",")).map(_.toInt).toList

  @tailrec
  def getCheapestCostPart1(minimum: Int, inputs: List[Int]): Int = {
    inputs match {
      case head :: tail =>
        val fuelCost =
          inputLines.filterNot(_ == head).map(fuel => (fuel - head).abs).sum
        if (fuelCost < minimum) {
          getCheapestCostPart1(fuelCost, tail)
        } else {
          getCheapestCostPart1(minimum, tail)
        }
      case Nil => minimum
    }
  }

  @tailrec
  def getCheapestCostPart2(minimum: Int, inputs: List[Int]): Int = {
    inputs match {
      case head :: tail =>
        val fuelCost =
          inputLines
            .filterNot(_ == head)
            .map { fuel =>
              val distance = (fuel - head).abs
              (0 to distance).sum
            }
            .sum
        if (fuelCost < minimum) {
          getCheapestCostPart2(fuelCost, tail)
        } else {
          getCheapestCostPart2(minimum, tail)
        }
      case Nil => minimum
    }
  }

  println(getCheapestCostPart1(Int.MaxValue, inputLines))

}
