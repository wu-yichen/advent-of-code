import Utils.readFrom

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 extends App {

  val inputLines = readFrom("Day10/Day10.txt").toList

  def part1 = {
    val stack = mutable.Stack.empty[Char]
    val points = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    val validPair = Map(')' -> '(', '}' -> '{', ']' -> '[', '>' -> '<')
    inputLines.flatMap { line =>
      line.toCharArray.map { ch =>
        if (validPair.contains(ch)) {
          if (stack.isEmpty || stack.pop() != validPair(ch)) {
            points(ch)
          } else {
            0
          }
        } else {
          stack.push(ch)
          0
        }
      }
    }.sum
  }

  def part2 = {
    val openChunks = Set('{', '[', '<', '(')
    val closeChunks = Set('}', ']', '>', ')')
    val validPair = Map(')' -> '(', '}' -> '{', ']' -> '[', '>' -> '<')
    val completePair = Map('(' -> ')', '{' -> '}', '[' -> ']', '<' -> '>')

    val result = inputLines
      .map { line =>
        val stack = mutable.Stack.empty[Char]
        line.toCharArray.map { ch =>
          if (openChunks.contains(ch)) {
            stack.push(ch)
          } else if (closeChunks.contains(ch)) {
            if (stack.nonEmpty && stack.top == validPair(ch)) {
              stack.pop()
            } else {
              stack.push(ch)
            }
          }
        }
        stack
      }
      .filter(_.forall(openChunks.contains))
      .map(_.map(completePair).toList)

    val finalResult = result
      .map(compute(_, 0, result.length))
      .sorted
    finalResult((result.length - 1) / 2)
  }

  @tailrec
  def compute(chs: Seq[Char], totalScore: Long, size: Int): Long = {
    val score = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    chs match {
      case Nil => totalScore
      case head :: tail =>
        compute(tail, score(head) + totalScore * 5, size)
    }
  }

  println(part2)

}
