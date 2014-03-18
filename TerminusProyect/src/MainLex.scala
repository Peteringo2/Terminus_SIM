import scala.io.Source._
object Lecture {

  def main(args: Array[String]): Unit = {
		Grammar.generateGrammar("Grammar.txt")
		println(Grammar.Grammar)
		
		val tokens: List[List[String]] = (Lex.analyze("file.txt"))
		println(tokens)
		println(SymbolTableGenerator.SymbolTable)
  }
}
