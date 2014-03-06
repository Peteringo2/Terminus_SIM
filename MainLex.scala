import scala.io.Source._
object Lecture {

  def main(args: Array[String]): Unit = {

		var tokens: List[List[String]] = List()
		tokens = tokens ::: (Lex.analyze("file.txt"))
		println(tokens)
  }
}
