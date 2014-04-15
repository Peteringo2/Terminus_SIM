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
		
		Lex.tokens = Lex.analyze("file.txt")
		
		val r = new BottomUp
		val root = new Nodo(0)	  
		root.addMap(r.getClosure("<S>", Grammar.Grammar("<S>"), 0))
		//println("inicial" + root.mapa)
		r.lista_nodos = Set(root)
		r.nodeClosureCreater(root)
		
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
		
		for(key <- r.tablaSLR.keys) println("llave: " + key + " --- " + r.tablaSLR(key))
		
		r SLRParse
		  
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
