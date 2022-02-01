import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Spec extends AnyFlatSpec with Matchers {
  private val input = Utils.readFrom("Day01/Day01-test.txt").map(_.toInt).toList

  "Day01" should "calculate the correct power consumption based on test input" in {
    Day01.calculateNumberOfIncreasingEntries(input) should ===(7)
  }

  it should "calculate the correct life support rating based on test input" in {
    Day01.calculateWithSlidingWindow(input) should ===(5)
  }
}
