object Utils {
  def readFrom(filename: String): Iterator[String] = {
    scala.io.Source.fromFile(s"src/main/resources/$filename").getLines()
  }
}
