
class BottomUp{  
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  var nodes_names = 1
  var lista_nodos : Set[Nodo] = Set()
  var listOfRoot = List[(String, String)]()
  var states = List[Int]()
  var symbols = List[String]()
  var tablaSLR = Map[(Int,String), String]()
  var cadena = List[String]()
  var root = new Nodo(0)
  
  def getClosure(charToClosure :String, start : Int): Map[Any, Set[(Any,Any)]] = {	  
	  var map : Map[Any, Set[(Any,Any)]] = Map()
	  map += (charToClosure -> Set())
	  for(str_chain <- Grammar.Grammar(charToClosure))
	  {
	     if(str_chain != "!") {
			var indices = getStringsToProcess(str_chain, start)
		    for(index <- indices){
				var temp =  map(charToClosure); temp = temp.union(Set((str_chain, index)));
				if(index < str_chain.length){
					str_chain charAt index match{
					case ('@') => map += (charToClosure -> temp)
					case ('<') => {	
					  map ++= (getClosure(Grammar.NonTerminal.findFirstIn(str_chain.substring(index)).mkString(""), 0))
					  var prod = map(charToClosure)
					  prod = prod.union(temp)
					  map += (charToClosure -> prod)

					  }
					case _ => List((1,1,1))
					}
				}
				else 
					map += (charToClosure -> temp)
			}	  
		}
	  }
  map
  }
  
  def getStringsToProcess(cadena : String, index : Int)= {
     var indexes = Set[Int]()//lista de indices a retornar
     if(index < cadena.length()){//entra si el indice es menor a la cadena
    	  	  var flag = cadena(index) == '<'//revisiar si es '<'
    	  	  var chain = tokenator.findFirstIn(cadena.substring(index)).mkString("")//obtenemos la cadena para ver si produce !
    	  	  indexes = indexes.union(Set(index))//añadimos el indice default a la lsita
			  while(flag){//si si es '<'
			      flag = false
				  if(Grammar.Grammar(chain).contains("!")){//si la key produce epsilon
					  indexes = indexes.union(Set(indexes.last + chain.length))//añadimos el nuevo indice a procesar a nuestro set
					  chain = tokenator.findFirstIn(cadena.substring(indexes.last)).mkString("")//obtenemos la siguiente cadena
					  if(indexes.last < cadena.length) flag = cadena(indexes.last) == '<'//revisamos si se sigue cumpliendo que x(0) == '<' para saber si regresar
				  }
			  }
	 }
     else 
       indexes = indexes.union(Set(index))
	 indexes
  }

  def nodeClosureCreater(n : Nodo) : Set[Nodo] = {
			  for(key <- n.mapa.keys; lista_tuples <- n.mapa(key)){
				  if(lista_tuples._2.asInstanceOf[Int] < lista_tuples._1.toString.length){
					  var letra_movimiento = tokenator.findFirstIn(lista_tuples._1.toString.substring(lista_tuples._2.asInstanceOf[Int])).mkString("")
					  var start = (letra_movimiento.length() + lista_tuples._2.asInstanceOf[Int])
					  
			    	  if(start <= lista_tuples._1.toString.length()){
			    	  var nuevo_nodo = new Nodo(nodes_names)
					  
					  nuevo_nodo.addMap(getClosure(key.toString, start))
					  
					  var name = equalsMaps(nuevo_nodo)
					  
					  if(name != -1) n.addToPointers((letra_movimiento, name))
					  else{ 
//					    println("sali del nodo: " + n.name + " llave " + key.toString + " movi: " + letra_movimiento)
//					  println("name: " + nuevo_nodo.name)
//					  println("metroid" + nuevo_nodo.mapa)
//					  println()
					    lista_nodos = lista_nodos.union(Set(nuevo_nodo))
					    nodes_names += 1
					    n.addToPointers((letra_movimiento, nuevo_nodo.name)) 
						var flag = nodoTermino(nuevo_nodo)
						if(flag){lista_nodos = lista_nodos.union(nodeClosureCreater(nuevo_nodo))}
					  }
			    	  
			    	  
			    	  }
				      
				  }
			  }

  lista_nodos
  }
  
  def equalsMaps(nodo : Nodo) : Int = {
	  for(n <- lista_nodos) if(n.mapa == nodo.mapa) return n.name
	  return -1
  }
  
  def nodoTermino(n : Nodo) : Boolean = {
    for(key <- n.mapa.keys; lista_tuples <- n.mapa(key))
    	if(lista_tuples._1.toString.length() < lista_tuples._2.asInstanceOf[Int] ) return false
    return true
  }
  
  
	def getTerminalAndNonTerminalForSLRTable{

		for( n <- lista_nodos; apuntador <-  n.pointers){
			tablaSLR += (n.name, apuntador._1) -> ("S"+apuntador._2.toString)
		}
	}

	def getReduceForSLRTable{
		for(n <- lista_nodos; key  <- n.mapa.keys ; prodThis <- n.mapa(key) if (prodThis._1.toString.length.toInt == prodThis._2) ; 
			follow <- Grammar.Follows(key toString) ; lineIndex <- 0 until listOfRoot.length if listOfRoot(lineIndex) == prodThis){
		  tablaSLR += (n.name, follow) -> ("R"+ lineIndex.toString)
		} 
	}	

	def SLRParse{
	  var flag = false
		states ::= 0
				cadena ::= "$"
				while(cadena.head != "$" && !flag){
					var key = (states.head, cadena.head)
							if(tablaSLR.exists(_==key)){
								tablaSLR(key).charAt(0) match {
								case 'S' => {
									states ::= tablaSLR(key).substring(1).toInt
											symbols ::= cadena.head
											cadena = cadena.tail
								}
								case 'R' => {

									val sizeOfProd = listOfRoot(tablaSLR(key).substring(1).toInt)._2.length

											for(symbol <- 0 until sizeOfProd){
												states = states.tail
														symbols = symbols.tail
											}
									symbols ::= listOfRoot(tablaSLR(key).substring(1).toInt)._1
											states ::= tablaSLR(states.head, symbols.head).toInt
								}
								}
							}
							else 
								flag = true;
				}
	  
	  if(symbols.head == "S") println("Aceptada")
	  else println("no aceptada")
	}

	def returnRootToList{
	  var productions : Set[(_,_)] = Set()
		for(key <- root.mapa.keys){
		   productions = root.mapa(key)
		   for(prod <- productions) listOfRoot :+= (key.toString, prod._1.toString)
		}
		 
	}
}