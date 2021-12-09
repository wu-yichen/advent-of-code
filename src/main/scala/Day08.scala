import Utils.readFrom

object Day08 extends App {
  case class Entry(signal: String, output: String)
  val inputLines =
    readFrom("Day08/Day08.txt").map { line =>
      line.split("\\|") match {
        case Array(signal, output) => Entry(signal, output)
      }
    }.toList

  def part1 = {
    inputLines
      .map(_.output)
      .map {
        _.split(" ")
          .count(digit => Seq(2, 3, 4, 7).contains(digit.trim.length))
      }
      .sum
  }

  def part2 = {
    val m = Map(2 -> 1, 4 -> 4, 3 -> 7, 7 -> 8)

    val signals = inputLines.map { input =>
      val signal = input.signal
      val s = signal.split(" ").toList.partition(d => m.contains(d.length))
      val unique =
        s._1.groupBy(d => m(d.length)).map(kv => (kv._1, kv._2.head))

      val notUnique = s._2.foldLeft(Map.empty[Int, String]) { (acc, signal) =>
        if (
          signal.length == 6 && unique(4)
            .forall(char => signal.contains(char.toString))
        ) {
          acc ++ Map(9 -> signal)
        } else if (
          signal.length == 6 && unique(7)
            .forall(char => signal.contains(char.toString))
        ) {
          acc ++ Map(0 -> signal)
        } else if (signal.length == 6) {
          acc ++ Map(6 -> signal)
        } else if (
          signal.length == 5 && unique(1)
            .forall(char => signal.contains(char.toString))
        ) {
          acc ++ Map(3 -> signal)
        } else if (
          signal.length == 5 && unique(4).forall(char =>
            s"$signal${unique(1)}".distinct
              .contains(char.toString)
          )
        ) {
          acc ++ Map(5 -> signal)
        } else if (signal.length == 5) {
          acc ++ Map(2 -> signal)
        } else {
          acc
        }
      }

      val signalMap = (unique ++ notUnique).map(kv => (kv._2.sorted, kv._1))
      val outputs = input.output
        .split(" ")
        .filter(_.nonEmpty)
        .toList

      outputs.map(str => signalMap(str.sorted)).mkString.toInt
    }.sum

    signals
  }

  println(part2)
}
