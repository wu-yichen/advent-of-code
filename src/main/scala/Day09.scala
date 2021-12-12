import Utils.readFrom

object Day09 extends App {

  val heightMapWithIndex = {
    readFrom("Day09/Day09.txt")
      .map {
        _.toCharArray.map(_.asDigit).toList
      }
      .toList
      .map(_.zipWithIndex)
  }

  def getColumn(floor: (Int, Int)) = {
    heightMapWithIndex
      .flatMap((d: Seq[(Int, Int)]) => d.filter(_._2 == floor._2))
      .map(_._1)
  }

  def isLowPointsInColumn(floor: (Int, Int), row: (List[(Int, Int)], Int)) = {
    val floorInColumn = getColumn(floor)
    val rowIndex = row._2
    if (rowIndex - 1 < 0) {
      floor._1 < floorInColumn(rowIndex + 1)
    } else if (rowIndex + 1 >= heightMapWithIndex.length) {
      floor._1 < floorInColumn(rowIndex - 1)
    } else {
      floor._1 < floorInColumn(rowIndex - 1) && floor._1 < floorInColumn(
        rowIndex + 1
      )
    }
  }

  def isLowPointsInRow(floor: (Int, Int), row: (List[(Int, Int)], Int)) = {
    val element = floor._1
    val elementIndex = floor._2
    val rowElement = row._1
    if (elementIndex - 1 < 0) {
      element < rowElement(elementIndex + 1)._1
    } else if (elementIndex + 1 >= rowElement.length) {
      element < rowElement(elementIndex - 1)._1
    } else {
      element < rowElement(elementIndex - 1)._1 && element < rowElement(
        elementIndex + 1
      )._1
    }
  }

  def getLowPoints(row: (List[(Int, Int)], Int)) =
    row._1.filter(floor =>
      isLowPointsInRow(floor, row) && isLowPointsInColumn(floor, row)
    )

  def part1 =
    heightMapWithIndex.zipWithIndex
      .flatMap(getLowPoints)
      .map(_._1 + 1)
      .sum

//  println(part1)

  def dfs(
      heightMap: Seq[Seq[(Int, Int)]],
      rowIndex: Int,
      columnIndex: Int,
      visited: Array[Array[Boolean]],
      count: Array[Int]
  ): Array[Int] = {
    if (
      !(0 <= rowIndex && rowIndex < heightMap.length) || !(0 <= columnIndex && columnIndex < heightMap.head.length) || heightMap(
        rowIndex
      )(columnIndex)._1 == 9 || visited(rowIndex)(columnIndex)
    ) {
      return count
    }
    visited(rowIndex)(columnIndex) = true
    count(0) += 1
    dfs(heightMap, rowIndex + 1, columnIndex, visited, count)
    dfs(heightMap, rowIndex - 1, columnIndex, visited, count)
    dfs(heightMap, rowIndex, columnIndex + 1, visited, count)
    dfs(heightMap, rowIndex, columnIndex - 1, visited, count)
  }

  def part2 = {

    val visited = Array.fill(
      heightMapWithIndex.length,
      heightMapWithIndex.head.length
    )(false)

    heightMapWithIndex.zipWithIndex
      .flatMap { rows: (List[(Int, Int)], Int) =>
        rows._1.map { row =>
          if (
            heightMapWithIndex(rows._2)(row._2)._1 < 9 && !visited(rows._2)(
              row._2
            )
          ) {
            dfs(
              heightMapWithIndex,
              rows._2,
              row._2,
              visited,
              Array(0)
            ).head
          } else 0
        }
      }
      .filter(_ > 0)
      .sortBy(-_)
      .take(3)
      .product

  }

  println(part2)
}
