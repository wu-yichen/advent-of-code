import Utils.readFrom

import scala.annotation.tailrec

object Day13 extends App {

  val transparentPaper =
    readFrom("Day13/Day13.txt").toList

  case class Dots(row: Int, col: Int)
  val markedDots =
    transparentPaper
      .filter(_.contains(','))
      .map(_.split(",") match {
        case Array(x, y) => Dots(y.toInt, x.toInt)
      })

  sealed trait Direction
  final case class Horizontal(position: Int) extends Direction
  final case class Vertical(position: Int) extends Direction

  val foldInstructions = {
    val pattern = "fold along (\\w)=([0-9]*)".r
    transparentPaper
      .filter(pattern.findFirstIn(_).isDefined)
      .map {
        case pattern(direction, position) =>
          direction match {
            case "y" => Horizontal(position.toInt)
            case "x" => Vertical(position.toInt)
          }
      }
  }

  @tailrec
  def doInstructions(
      paper: Array[Array[Char]],
      foldInstructions: Seq[Direction]
  ): Array[Array[Char]] = {
    foldInstructions match {
      case head :: tail => doInstructions(startOrigami(head, paper), tail)
      case Nil          => paper
    }
  }

  def startOrigami(instruction: Direction, paper: Array[Array[Char]]) = {
    instruction match {
      case horizontal: Horizontal =>
        foldUp(
          horizontal.position,
          horizontal.position - 1,
          horizontal.position + 1,
          paper
        )
      case vertical: Vertical =>
        paper.map(
          foldLeft(
            _,
            vertical.position,
            vertical.position - 1,
            vertical.position + 1
          )
        )
    }
  }

  @tailrec
  def foldLeft(
      line: Array[Char],
      baseLine: Int,
      currentPosition: Int,
      targetPosition: Int
  ): Array[Char] = {
    if (targetPosition == line.length) {
      line.take(baseLine)
    } else if (currentPosition == 0 && targetPosition < line.length - 1) {
      line.takeRight(line.length - targetPosition) ++ line.take(baseLine)
    } else {
      if (line(targetPosition) == '#')
        line(currentPosition) = line(targetPosition)
      foldLeft(line, baseLine, currentPosition - 1, targetPosition + 1)
    }
  }

  @tailrec
  def foldUp(
      baseLine: Int,
      currentLine: Int,
      targetLine: Int,
      paper: Array[Array[Char]]
  ): Array[Array[Char]] = {

    if (targetLine == paper.length) {
      paper.splitAt(baseLine)._1
    } else if (currentLine == 0 && targetLine < paper.length - 1) {
      paper.takeRight(paper.length - targetLine) ++ paper.splitAt(baseLine)._1
    } else {
      val positionVisible =
        paper(targetLine).zipWithIndex.filter(_._1 == '#').map(_._2)

      if (positionVisible.nonEmpty) {
        positionVisible.foreach(position => paper(currentLine)(position) = '#')
      }

      foldUp(baseLine, currentLine - 1, targetLine + 1, paper)
    }
  }

  def initialPaper = {
    val filledHorizontal = markedDots.map(_.row).max + 1
    val filledVertical = markedDots.map(_.col).max + 1
    val paper = Array.fill(filledHorizontal, filledVertical)('.')
    markedDots.foreach { dot =>
      paper(dot.row)(dot.col) = '#'
    }
    paper
  }

  def part1 = {
    startOrigami(foldInstructions.head, initialPaper).map(_.count(_ == '#')).sum
  }

  def part2 = {
    doInstructions(initialPaper, foldInstructions).foreach(d =>
      println(d.mkString(" "))
    )
  }

  part2

}
