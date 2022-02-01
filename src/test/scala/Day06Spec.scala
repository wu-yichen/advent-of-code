import Utils.readFrom
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Spec extends AnyFlatSpec with Matchers {
  val inputLines =
    readFrom("Day06/Day06-test.txt").flatMap(_.split(",")).map(_.toInt).toVector

  "Day06" should "calculate the correct number of lantern fish after 80 days based on test input" in {
    Day06.compute(inputLines, 80) should ===(5934)
  }

  it should "calculate the correct number of lantern fish after 256 days based on test input" in {
    Day06.computePart2(inputLines, 256) should ===(26984457539L)
  }
}
