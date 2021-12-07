import Utils.readFrom

import scala.annotation.tailrec
import scala.collection.mutable

object Day06 extends App {

  val inputLines =
    readFrom("Day06/Day06.txt").flatMap(_.split(",")).map(_.toInt).toVector

  @tailrec
  def compute(inputs: Seq[Int], days: Int): Int = {
    if (days == 0) {
      inputs.size
    } else {
      val numOfEight = inputs.count(_ == 0)
      val newInputs = inputs.map {
        case timer if timer == 0 => 6
        case timer               => timer - 1
      }
      compute(newInputs ++ Seq.fill(numOfEight)(8), days - 1)
    }
  }

  def computePart2(inputs: Seq[Int], days: Int) = {
    val baseMap = Map(
      0L -> 0L,
      1L -> 0L,
      2L -> 0L,
      3L -> 0L,
      4L -> 0L,
      5L -> 0L,
      6L -> 0L,
      7L -> 0L,
      8L -> 0L
    )
    var fishMap = mutable.HashMap[Long, Long]()

    fishMap ++= baseMap

    inputs.foreach(fishMap(_) += 1)

    (0 until days).foreach { _ =>
      val newFishMap = mutable.HashMap[Long, Long]()
      newFishMap ++= baseMap
      fishMap.foreach {
        case (0, _) =>
          newFishMap(6) = fishMap(0) + newFishMap(6)
          newFishMap(8) = fishMap(0) + newFishMap(8)
        case (n, _) =>
          newFishMap(n - 1) = fishMap(n) + newFishMap(n - 1)
      }
      fishMap = newFishMap
    }
    fishMap.values.sum
  }

  println(computePart2(inputLines, 256))
}
