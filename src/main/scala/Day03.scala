import Utils.readFrom

import scala.annotation.tailrec

object Day03 extends App {

  val inputLines = readFrom("Day03/Day03.txt").toList

  def calculatePowerConsumption(inputLines: List[String]) = {
    val (gamma, epsilon) = compute(inputLines)
      .foldLeft(("", "")) { (acc, rate) =>
        val gammaBit = rate.head._1
        val epsilonBit = rate.last._1
        (s"${acc._1}$gammaBit", s"${acc._2}$epsilonBit")
      }

    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)

  }

  def calculateLifeSupportRating(input: List[String]): Int = {
    @tailrec
    def filter(
        bits: List[String],
        isO2: Boolean = true,
        index: Int = 0
    ): String = {
      val ones: Double =
        bits.map(_.charAt(index).toString.toInt).sum.doubleValue
      val matchBit: Char = (isO2, ones.compareTo(bits.length / 2.0)) match {
        case (true, 1)   => '1' // O2 and most common bit is '1', match '1'
        case (true, -1)  => '0' // O2 and most common bit is '0', match '0'
        case (true, _)   => '1' // O2 and equal '0's and '1's, match '1'
        case (false, 1)  => '0' // CO2 and most common bit is '1', match '0'
        case (false, -1) => '1' // CO2 and most common bit is '0', match '1'
        case (false, _)  => '0' // CO2 and equal '0' and '1', match '0'
      }

      val filtered: List[String] = bits.filter(s => s.charAt(index) == matchBit)
      if (filtered.length != 1) filter(filtered, isO2, index + 1)
      else filtered.head
    }

    val oxygen = filter(input)
    val co2 = filter(input, isO2 = false)
    Integer.parseInt(oxygen, 2) * Integer.parseInt(co2, 2)
  }

  private def calculateOccurrences(input: List[Char]) = {
    input
      .groupBy(identity)
      .map { case (bit, times) =>
        (bit, times.size)
      }
      .toList
      .sortBy(-_._2)
  }

  private def compute(inputLines: List[String]) = {
    inputLines
      .map(_.toList)
      .transpose
      .map(calculateOccurrences)
  }
}
