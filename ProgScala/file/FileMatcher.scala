import java.io._

object FileMatcher {

  private def filesHere = (new java.io.File(".")).listFiles

  def filesEnding(query: String) =
    for {
      f <- filesHere
      if f.getName.endsWith(query)
    } yield f.getName


  def main(argv: Array[String]): Unit = {
    val fs = filesEnding("moon")
    println(fs.get)
  }

}

