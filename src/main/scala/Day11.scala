import Utils.readFrom

import scala.annotation.tailrec

object Day11 extends App {

  case class Energy(value: Int, rowIndex: Int, colIndex: Int)

  val energyLevel = {
    readFrom("Day11/Day11.txt")
      .map {
        _.toCharArray.map(_.asDigit).toList
      }
      .toList
      .map(_.zipWithIndex)
      .zipWithIndex
      .map(toEnergy)
  }

  def toEnergy(element: (List[(Int, Int)], Int)) = {
    val energiesWithIndex = element._1
    val rowIndex = element._2
    energiesWithIndex.map(e => Energy(e._1, rowIndex, e._2))
  }

  case class Accumulator(
      energyLevel: List[List[Energy]],
      numOfZero: Int = 0
  )

  def part1 = {
    (0 until 100)
      .foldLeft(Accumulator(energyLevel)) { (accumulator, _) =>
        val newEnergyLevel =
          flashIfGreaterThanNine(
            accumulator.energyLevel.map(setEnergyLevel).map(_.toArray).toArray,
            Array.fill(10, 10)(false)
          ).map(_.toList).map(resetToZero).toList
        Accumulator(
          newEnergyLevel,
          accumulator.numOfZero + newEnergyLevel.map(_.count(_.value == 0)).sum
        )
      }
      .numOfZero
  }

  def resetToZero(energies: List[Energy]) = {
    energies.map(energy =>
      if (energy.value > 9) energy.copy(value = 0) else energy
    )
  }

  def setEnergyLevel(row: Seq[Energy]) = {
    row
      .map(increaseByOne)
      .toList
  }

  def increaseByOne(energy: Energy) = {
    energy.copy(value = energy.value + 1)
  }

  @tailrec
  def flashIfGreaterThanNine(
      energyLevel: Array[Array[Energy]],
      flashed: Array[Array[Boolean]]
  ): Array[Array[Energy]] = {

    val positionsNeedToIncrease = energyLevel.flatMap { energies =>
      energies.collect {
        case energy
            if energy.value > 9 && !flashed(energy.rowIndex)(
              energy.colIndex
            ) =>
          flashed(energy.rowIndex)(energy.colIndex) = true
          getAllAdjacent(energy)
      }.flatten
    }

    if (positionsNeedToIncrease.isEmpty) {
      energyLevel
    } else {
      flashIfGreaterThanNine(
        increaseByOneForOthers(
          positionsNeedToIncrease,
          energyLevel,
          flashed
        ),
        flashed
      )
    }
  }

  def increaseByOneForOthers(
      adjacentNeedToIncrease: Seq[Position],
      energyLevel: Array[Array[Energy]],
      flashed: Array[Array[Boolean]]
  ) = {

    adjacentNeedToIncrease.foreach { position =>
      if (!flashed(position.rowIndex)(position.colIndex)) {
        val energy = energyLevel(position.rowIndex)(position.colIndex)
        energyLevel(position.rowIndex)
          .update(position.colIndex, increaseByOne(energy))
      }
    }

    energyLevel
  }

  def getAllAdjacent(
      energy: Energy
  ) = {

    val left = Position.fromIndex(energy.rowIndex, energy.colIndex - 1)
    val right = Position.fromIndex(energy.rowIndex, energy.colIndex + 1)
    val top = Position.fromIndex(energy.rowIndex - 1, energy.colIndex)
    val topLeft = Position.fromIndex(energy.rowIndex - 1, energy.colIndex - 1)
    val topRight = Position.fromIndex(energy.rowIndex - 1, energy.colIndex + 1)
    val bottom = Position.fromIndex(energy.rowIndex + 1, energy.colIndex)
    val bottomLeft =
      Position.fromIndex(energy.rowIndex + 1, energy.colIndex - 1)
    val bottomRight =
      Position.fromIndex(energy.rowIndex + 1, energy.colIndex + 1)

    Seq(left, right, top, topLeft, topRight, bottom, bottomLeft, bottomRight)
      .collect {
        case Some(position) => position
      }

  }

  sealed abstract case class Position private (rowIndex: Int, colIndex: Int)
  object Position {
    def fromIndex(rowIndex: Int, colIndex: Int): Option[Position] = {
      if (
        !(0 <= rowIndex && rowIndex < 10) || !(0 <= colIndex && colIndex < 10)
      ) None
      else Some(new Position(rowIndex, colIndex) {})
    }
  }

//  print(part1)

  @tailrec
  def part2(step: Int, energyLevel: Seq[Seq[Energy]]): Int = {
    if (energyLevel.forall(_.forall(_.value == 0))) {
      step
    } else {
      val newEnergyLevel =
        flashIfGreaterThanNine(
          energyLevel.map(setEnergyLevel).map(_.toArray).toArray,
          Array.fill(10, 10)(false)
        ).map(_.toList).map(resetToZero).toList
      part2(step + 1, newEnergyLevel)
    }
  }

  print(part2(0, energyLevel))

}
