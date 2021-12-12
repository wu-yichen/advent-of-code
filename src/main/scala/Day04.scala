import Utils.readFrom

object Day04 extends App {

  case class BingoNumber(number: Int, var marked: Boolean = false)

  case class BingoCard(card: List[BingoNumber]) {
    val rows = card.grouped(5).toList
    val columns = card.zipWithIndex.groupBy(_._2 % 5).map(_._2.map(_._1))
  }

  def readCard(lines: List[String]): BingoCard =
    BingoCard(lines.flatMap(_.grouped(3)).map(_.trim.toInt).map(BingoNumber(_)))

  def markCard(number: Int, card: BingoCard) =
    card.card.filter(_.number == number).foreach(bg => bg.marked = true)

  def lineBingo(line: List[BingoNumber]): Boolean =
    line.count(_.marked) == line.size

  def bingo(bingoCard: BingoCard): Boolean =
    bingoCard.rows.exists(lineBingo) || bingoCard.columns.exists(lineBingo)

  def calculateScore(lastDraw: Int, card: BingoCard) =
    card.card.filterNot(_.marked).map(_.number).sum * lastDraw

  val input = readFrom("Day04/Day04.txt").toList

  val draw = input.head.split(",").map(_.toInt).toList

  val cards = input.drop(2).grouped(6).map(_.take(5)).map(readCard).toList

  var remainingCards = cards

  val drawScore = draw.map(dr => {
    remainingCards.foreach(c => markCard(dr, c))
    val score = remainingCards
      .find(bingo)
      .map(winningCard => calculateScore(dr, winningCard))
    remainingCards = remainingCards.filterNot(bingo)
    (dr, score)
  })

  println(
    s"Part1: ${drawScore.find(_._2.isDefined).map(_._2.get).getOrElse("")}"
  )
  println(
    s"Part2: ${drawScore.reverse.find(_._2.isDefined).map(_._2.get).getOrElse("")}"
  )

}
