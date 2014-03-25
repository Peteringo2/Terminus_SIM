
class BottomUp{  
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  
	def getClosure(charToClosure :String): Map[Any, List[(Any,Any)]] = {
			var map : Map[Any, List[(Any,Any)]] = Map()
			map += (charToClosure -> List())
			for(str_chain <- Grammar.Grammar(charToClosure))
			{
			  if(str_chain != "!") {
						//var lista_a_procesar = List(tokenator.findFirstIn(str_chain).mkString(""))
						var indices = getStringsToProcess(str_chain)
						for(index <- indices){
						str_chain charAt index match{
						case ('@') =>{var temp =  map(charToClosure); temp = temp ::: List((str_chain, index)); map += (charToClosure -> temp)}
						case ('<') => { var temp =  map(charToClosure); temp = temp ::: List((str_chain, index)); map += (charToClosure -> temp)
						  map ++= getClosure(Grammar.NonTerminal.findFirstIn(str_chain.substring(index)).mkString(""))
						}
						case _ => List((1,1,1))
						}
						}
			 }
			}
			map
	}
  
	def getStringsToProcess(cadena : String)= {
	    var cadenas = List(tokenator.findFirstIn(cadena).mkString(""))
	    var indexes = List(0)
			var flag = cadenas.last(0) == '<'
			while(flag){
				if(Grammar.Grammar(cadenas.last).contains("!")){
				  indexes :+= indexes.last + cadenas.last.length()
				  println(indexes)
				  cadenas :+= tokenator.findFirstIn(cadena.substring(cadenas.last.length)).mkString("")
				}
				else flag = false
			}
	    indexes
	}


	def nodeClosureCreater(prod : String, name : Int){
		val n = new Nodo(name)
		n.addMap(getClosure(prod))

		for(x <- n.mapa.keys){
			println("metroid "+ x + " -> " + n.mapa(x))
		}
	}


}