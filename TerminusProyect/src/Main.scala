import scala.io.Source._
object Lecture {

  def main(args: Array[String]){
		Grammar.generateGrammar("Grammar.txt")
		
		println("Gramatica:")
		println(Grammar.Grammar + "\n\n")
		println("los firsts son: ")
		println(Grammar.getFirsts+ "\n\n")
		
		val tokens: List[List[String]] = (Lex.analyze("file.txt"))
		println(tokens)
		println(SymbolTableGenerator.SymbolTable)
		
		ErrorLog.S.close()
  }
}
