import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {
  private val input: List[String] =
    Utils.readFrom("Day03/Day03-test.txt").toList

  "Day03" should "calculate the correct power consumption based on test input" in {
    Day03.calculatePowerConsumption(input) should ===(198)
  }

  it should "calculate the correct life support rating based on test input" in {
    Day03.calculateLifeSupportRating(input) should ===(230)
  }
}
