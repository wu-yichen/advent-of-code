import Utils.readFrom

object Day01 extends App {
  val inputLines = readFrom("Day01/Day01.txt").map(_.toInt).toList

  def calculateNumberOfIncreasingEntries(inputLines: List[Int]) = {
    inputLines.sliding(2).count(list => list(1) > list.head)
  }

  def calculateWithSlidingWindow(inputLines: List[Int]) = {
    calculateNumberOfIncreasingEntries(inputLines.sliding(3).map(_.sum).toList)
  }
}
