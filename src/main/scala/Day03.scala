import Utils.readFrom

object Day03 extends App {

  val inputLines = readFrom("Day03/Day03.txt").toList

  def calculateOccurrences(input: List[Char]) = {
    input
      .groupBy(identity)
      .map {
        case (bit, times) => (bit, times.size)
      }
      .toList
      .sortBy(-_._2)
  }

  def compute(inputLines: List[String]) = {
    inputLines
      .map(_.toList)
      .transpose
      .map(calculateOccurrences)
  }

  def getConsumption(inputLines: List[String]) = {
    val (gamma, epsilon) = compute(inputLines)
      .foldLeft(("", "")) { (acc, rate) =>
        val gammaBit = rate.head._1
        val epsilonBit = rate.last._1
        (s"${acc._1}$gammaBit", s"${acc._2}$epsilonBit")
      }

    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)

  }

  println(getConsumption(inputLines))
}
