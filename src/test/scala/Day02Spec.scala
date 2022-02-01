import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {
  private val input: List[String] =
    Utils.readFrom("Day02/Day02-test.txt").toList

  "Day02" should "calculate correct horizontal position and depth ignoring aim" in {
    Day02.getFinalDepth(input) should ===(150)
  }

  it should "calculate correct horizontal position and depth taking aim into account" in {
    Day02.getFinalDepthWithAim(input) should ===(900)
  }
}
