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
//		println(Grammar.getFollows + "\n\n")
		
		val r = new BottomUp  
		r.root.addMap(r.getClosure("<S>", 0))
		//println("inicial" + root.mapa)
		r.lista_nodos = Set(r.root)
		r.nodeClosureCreater(r.root)
		
		println("nodos")
		for(x <- r.lista_nodos){
		  println("Indice: " + x.name)
		  println("Punteros: " + x.pointers)
		  println("Mapa: " + x.mapa)
		  println()
		}
		
		r returnRootToList
		
		r getTerminalAndNonTerminalForSLRTable
		
		r getReduceForSLRTable
		
		println(r tablaSLR)
		
//		b SLRParse
		
		
		
		
		
		
		  
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
