import Utils.readFrom

object Day02 extends App {

  val inputLines = readFrom("Day02/Day02.txt").toList

  def getFinalDepth(inputLines: List[String]) = {
    inputLines
      .map { line =>
        line.split("\\s+") match {
          case Array(direction, step) => (direction, step.toInt)
        }
      }
      .foldLeft((0, 0)) { (acc, commands) =>
        commands match {
          case ("forward", step) => (acc._1 + step, acc._2)
          case ("down", step)    => (acc._1, acc._2 + step)
          case ("up", step)      => (acc._1, acc._2 - step)
        }
      } match {
      case (horizontal, depth) => horizontal * depth
    }
  }

  def getFinalDepthWithAim(inputLines: List[String]) = {
    inputLines
      .map { line =>
        line.split("\\s+") match {
          case Array(direction, step) => (direction, step.toInt)
        }
      }
      .foldLeft((0, 0, 0)) { (acc, commands) =>
        commands match {
          case ("forward", step) if acc._3 == 0 =>
            (acc._1 + step, acc._2, acc._3)
          case ("forward", step) =>
            (acc._1 + step, acc._2 + acc._3 * step, acc._3)
          case ("down", step) => (acc._1, acc._2, acc._3 + step)
          case ("up", step)   => (acc._1, acc._2, acc._3 - step)
        }
      } match {
      case (horizontal, depth, _) => horizontal * depth
    }
  }
}
