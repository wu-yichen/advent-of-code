import Utils.readFrom

object Day05 extends App {

  case class Coordinate(x: Int, y: Int)
  case class Line(first: Coordinate, second: Coordinate)

  val pattern = "([0-9]*),([0-9]*) -> ([0-9]*),([0-9]*)".r
  val inputLines =
    readFrom("Day05/Day05.txt").map {
      case pattern(startX, startY, endX, endY) =>
        Line(
          Coordinate(startX.toInt, startY.toInt),
          Coordinate(endX.toInt, endY.toInt)
        )
    }.toList

  def distance(fullMetrics: Array[Array[Int]], line: Line) =
    line match {
      case Line(first, second) if first.x == second.x =>
        (for {
          x <- 0 until 1000
          y <- 0 until 1000
          if x == first.x && (Math
            .min(first.y, second.y) to Math.max(first.y, second.y)).contains(y)
        } yield {
          fullMetrics(y)(x) += 1
          fullMetrics
        }).last

      case Line(first, second) if first.y == second.y =>
        (for {
          x <- 0 until 1000
          y <- 0 until 1000
          if y == first.y && (Math
            .min(first.x, second.x) to Math.max(first.x, second.x)).contains(x)
        } yield {
          fullMetrics(y)(x) += 1
          fullMetrics
        }).last

      case _ => fullMetrics
    }

  def distanceWithDiagonal(fullMetrics: Array[Array[Int]], line: Line) =
    line match {
      case Line(first, second)
          if (first.x - second.x).abs == (first.y - second.y).abs =>
        val minX = Math.min(first.x, second.x)
        val maxX = Math.max(first.x, second.x)
        val minY = Math.min(first.y, second.y)
        val maxY = Math.max(first.y, second.y)
        (for {
          x <- minX to maxX
          y <- minY to maxY
          if ((x - y).abs == (first.x - first.y).abs && (first.x - x).abs == (first.y - y).abs) || x + y == first.x + first.y
        } yield {
          fullMetrics(y)(x) += 1
          fullMetrics
        }).last

      case _ => fullMetrics
    }

  def part1(inputs: Seq[Line]) = {
    val fullMetrics = Array.fill[Int](1000, 1000)(0)
    inputs.foldLeft(fullMetrics)(distance)
  }

  def part2(inputs: Seq[Line]) = {
    val firstPartMetrics = part1(inputs)
    inputs.foldLeft(firstPartMetrics)(distanceWithDiagonal)
  }

  def compute(finalMetrics: Array[Array[Int]]) = {
    finalMetrics.map(_.count(_ >= 2)).sum
  }

//  println(compute(part1(inputLines)))
  println(compute(part2(inputLines)))
}
