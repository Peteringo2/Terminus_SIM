import scala.io.Source._
object Lecture {

  def main(args: Array[String]): Unit = {
		Grammar.generateGrammar("Grammar.txt")
		println("Gramatica:")
		println(Grammar.Grammar + "\n\n")
		println("los firsts son: ")
		Grammar.getFirsts
		println(Grammar.Firsts + "\n\n")
		println("los follows:")
		println(Grammar.getFollows + "\n\n")
		
		val r = new BottomUp
		r.nodeClosureCreater("<S>", 0)
//		TopDown.generateTable
//		println("La tabla top Down es: ")
//		println(TopDown.Table + "\n\n")
//		
//		Lex.tokens = (Lex.analyze("file.txt"))
//		println("tokens: \n" + Lex.tokens)
//		println("Symbol Table: \n" + SymbolTableGenerator.SymbolTable)
		
		//println(TopDown.Table(Tuple2("<content>","DO")))
		
		//println(TopDown.SearchLL)
		
		ErrorLog.S.close()
  }
}
