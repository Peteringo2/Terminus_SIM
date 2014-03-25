
class BottomUp{  
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  
	def getClosure(charToClosure :String, index : Int): Map[Any, List[(Any,Any)]] = {
			var map : Map[Any, List[(Any,Any)]] = Map()
			map += (charToClosure -> List())
			for(str_chain <- Grammar.Grammar(charToClosure))
			{
						var lista_a_procesar = List(tokenator.findFirstIn(str_chain).mkString(""))
						if(str_chain != "!") lista_a_procesar = getStringsToProcess(str_chain)
						for(cadena <- lista_a_procesar){
						cadena charAt index match{
						case ('@') =>{var temp =  map(charToClosure); temp = temp ::: List((str_chain, 0)); map += (charToClosure -> temp)}
						case ('<') => { var temp =  map(charToClosure); temp = temp ::: List((str_chain, 0)); map += (charToClosure -> temp)
						  map ++= getClosure(cadena, 0)
						}
						case _ => List((1,1,1))
						}
						}
			}
			map
	}
  
	def getStringsToProcess(cadena : String)= {
	    var cadenas = List(tokenator.findFirstIn(cadena).mkString(""))
			var flag = cadenas.last(0) == '<'
			while(flag)
				if(Grammar.Grammar(cadenas.last).contains("!")) cadenas :+= tokenator.findFirstIn(cadena.substring(cadenas.last.length)).mkString("")
			  else flag = false
			cadenas
	}


	def nodeClosureCreater(prod : String, name : Int){
		val n = new Nodo(name)
		n.addMap(getClosure(prod, 0))

		for(x <- n.mapa.keys){
			println("metroid "+ x + " -> " + n.mapa(x))
		}
	}


}