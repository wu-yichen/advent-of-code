import Utils.readFrom

object Day04 extends App {

  val (numbers, allBoards) = {

    val inputLines = readFrom("Day04/Day04-test.txt").toList

    val randomOrderNumber =
      inputLines.takeWhile(_.nonEmpty)

    val allBoards = inputLines
      .diff(randomOrderNumber)
      .grouped(6)
      .map(_.drop(1))
      .map(transBoardInRow)
      .toList

    (randomOrderNumber.head.split(",").map(_.toInt).toList, allBoards)
  }

  def transBoardInRow(row: Seq[String]) = {
    row
      .flatMap(_.trim.split("\\s+").map(_.toInt).zipWithIndex)
      .toList
      .grouped(5)
      .toList
  }

  def findWinning(
      matchedNumberIndexRecords: Seq[Seq[Seq[(Int, Int)]]],
      startIndex: Int
  ) = {

//    val winningBoardIndexRecord =
//      matchedNumberIndexRecords.find((board: Seq[Seq[Int]]) =>
//        board.contains(Seq(0, 1, 2, 3, 4))
//      )
//
//    if (winningBoardIndexRecord.isDefined) {
//
//      //      (winningBoard, startIndex - 1, matchedNumberIndexRecords)
//    } else if (startIndex < numbers.size) {

    val newMatchedForAllBoards = {
      val indexes = for {
        board: Seq[List[(Int, Int)]] <- allBoards
        row: Seq[(Int, Int)] <- board
      } yield row.filter(_._1 == numbers(startIndex)).map(_._2)

      indexes
    }

    newMatchedForAllBoards
//    findWinning(newMatchedNumberIndexForAllBoards, startIndex + 1)

//    } else {
//      (None, -100, Seq.empty[Board])
//    }
  }

//  def getMatchedNumbersIndexForBoard(
//      board: Seq[Seq[(Int, Int)]],
//      numbers: List[Int],
//      startIndex: Int
//  ) = {
//    board.map(getMatchedNumbersWithIndex(_, numbers, startIndex))
//  }
//
//  def getMatchedNumbersWithIndex(rows: Seq[(Int, Int)], numbers: List[Int], startIndex: Int) = {
//    rows.zipWithIndex.filter(_._1 == numbers(startIndex))
//  }
//
//  def getUnMarkedNumbers(
//      currentBoards: Seq[Board],
//      startIndex: Int
//  ) = {
//    for {
//      board <- currentBoards
//    } yield board.map(_.filter(_ != numbers(startIndex)))
//  }

//  @tailrec
//  def findLastMatchedBoard(
//      boards: Seq[Board],
//      index: Int
//  ): (Option[Board], Int) = {
//
//    val winningResult = findWinning(numbers, boards, index)
//    val winningBoard: Option[Board] = winningResult._1
//    val winningIndex = winningResult._2
//
//    if (winningBoard.isDefined && boards.size == 1) {
//      (winningResult._1, winningResult._2)
//    } else {
//      val allLeftBoards = winningResult._3
//      winningBoard.map(wb =>
//        allLeftBoards.filterNot((leftBoards: Seq[Row]) => leftBoards == wb)
//      ) match {
//        case Some(leftBoards) =>
//          findLastMatchedBoard(numbers, leftBoards, winningIndex)
//        case None =>
//          findLastMatchedBoard(numbers, boards.map(_.transpose), winningIndex)
//      }
//    }
//  }

  def playBingoPart1 = {
    findWinning(Seq(Seq(Seq.empty[(Int, Int)])), 0)
//    computeResult((result._1, result._2))
  }

//  def playBingoPart2 = {
//
//    val result = findLastMatchedBoard(allBoards, 0)
//
//    computeResult(result)
//  }

//  def computeResult(result: (Option[Board], Int)) = {
//    result._1
//      .map(_.flatten)
//      .map(_.sum)
//      .map(numbers(result._2) * _)
//      .getOrElse(0)
//  }

  println(playBingoPart1)
}
